package verdict

enum EvalError:
  case BadPath(path: FieldPath, error: PathError)
  case ShapeMismatch(path: FieldPath, expected: FieldType, found: String)
  case WrongType(path: FieldPath, operator: String, expected: FieldType, actual: FieldType)

  def message: String = this match
    case BadPath(path, error) => error.message(path)
    case ShapeMismatch(path, expected, found) =>
      s"${path.render}: schema says ${expected.render} but the record holds a $found"
    case WrongType(path, operator, expected, actual) =>
      s"${path.render}: $operator needs a ${expected.render} field, but ${path.render} is a ${actual.render}"

object Evaluator:

  /** Evaluates `rule` against `record`, building the explanation as it goes. */
  def evaluate[A <: Product](rule: Rule, record: A, schema: Schema[A]): Either[EvalError, Evidence] =
    go(rule, record, schema)

  /** The verdict is read back off the evidence rather than computed beside it,
    * so an explanation that says PASS can never accompany a `false`.
    */
  def holds[A <: Product](rule: Rule, record: A, schema: Schema[A]): Either[EvalError, Boolean] =
    evaluate(rule, record, schema).map(_.held)

  private def go(rule: Rule, record: Product, schema: Schema[?]): Either[EvalError, Evidence] =
    rule match
      case Rule.And(l, r) =>
        for
          le <- go(l, record, schema)
          re <- go(r, record, schema)
        // Both sides are always evaluated: short-circuiting would leave holes in
        // the explanation, which is the thing being produced.
        yield Evidence.All(conjuncts(le) ++ conjuncts(re))

      case Rule.Or(l, r) =>
        for
          le <- go(l, record, schema)
          re <- go(r, record, schema)
        yield Evidence.Any(disjuncts(le) ++ disjuncts(re))

      case Rule.Not(inner) =>
        go(inner, record, schema).map(Evidence.Negation.apply)

      case Rule.Gt(path, expected)  => compare(path, record, schema, ">", expected)(_ > _)
      case Rule.Gte(path, expected) => compare(path, record, schema, ">=", expected)(_ >= _)
      case Rule.Lt(path, expected)  => compare(path, record, schema, "<", expected)(_ < _)
      case Rule.Lte(path, expected) => compare(path, record, schema, "<=", expected)(_ <= _)

      case Rule.EqStr(path, expected) =>
        text(path, record, schema, "==").map { actual =>
          leaf(path, actual, "==", expected, actual == expected)
        }

      case Rule.In(path, expected) =>
        text(path, record, schema, "in").map { actual =>
          val rendered = if expected.isEmpty then "{}" else expected.toList.sorted.mkString("{", ", ", "}")
          leaf(path, actual, "in", rendered, expected.contains(actual))
        }

      case Rule.IsTrue(path) =>
        read(path, record, schema).flatMap {
          case FieldValue.Flag(actual) => Right(leaf(path, actual.toString, "is", "true", actual))
          case other                   => Left(EvalError.WrongType(path, "is true", FieldType.Bool, other.fieldType))
        }

  // `a && b && c` parses as `And(And(a, b), c)`, but a reader wants one list of
  // three conditions, not a leaning tree. Merging only happens between nodes of
  // the same connective, so `And(a, Or(b, c))` keeps its shape.
  private def conjuncts(evidence: Evidence): List[Evidence] = evidence match
    case Evidence.All(children) => children
    case other                  => List(other)

  private def disjuncts(evidence: Evidence): List[Evidence] = evidence match
    case Evidence.Any(children) => children
    case other                  => List(other)

  private def leaf(path: FieldPath, actual: String, operator: String, expected: String, held: Boolean): Evidence =
    Evidence.Leaf(s"${path.render} ($actual) $operator $expected", held)

  private def compare(path: FieldPath, record: Product, schema: Schema[?], operator: String, expected: BigDecimal)(
      test: (BigDecimal, BigDecimal) => Boolean
  ): Either[EvalError, Evidence] =
    read(path, record, schema).flatMap {
      case FieldValue.Num(actual) =>
        Right(leaf(path, FieldValue.Num(actual).render, operator, FieldValue.Num(expected).render, test(actual, expected)))
      case other =>
        Left(EvalError.WrongType(path, operator, FieldType.Number, other.fieldType))
    }

  private def text(path: FieldPath, record: Product, schema: Schema[?], operator: String): Either[EvalError, String] =
    read(path, record, schema).flatMap {
      case FieldValue.Text(actual) => Right(actual)
      case other                   => Left(EvalError.WrongType(path, operator, FieldType.Text, other.fieldType))
    }

  private def read(path: FieldPath, record: Product, schema: Schema[?]): Either[EvalError, FieldValue] =
    schema.resolve(path).left.map(EvalError.BadPath(path, _)).flatMap { steps =>
      def walk(current: Product, remaining: List[(Int, Field)]): Either[EvalError, FieldValue] =
        remaining match
          case Nil => Left(EvalError.BadPath(path, PathError.Unknown(path.render, schema.typeName, schema.fieldNames)))
          case (idx, field) :: rest =>
            val raw = current.productElement(idx)
            (field.tpe, rest) match
              case (FieldType.Nested(_), _ :: _) =>
                raw match
                  case nested: Product => walk(nested, rest)
                  case other           => Left(EvalError.ShapeMismatch(path, field.tpe, describe(other)))
              case (FieldType.Nested(inner), Nil) =>
                Left(EvalError.WrongType(path, "compare", FieldType.Text, FieldType.Nested(inner)))
              case (FieldType.Text, _) =>
                raw match
                  case s: String => Right(FieldValue.Text(s))
                  case other     => Left(EvalError.ShapeMismatch(path, FieldType.Text, describe(other)))
              case (FieldType.Bool, _) =>
                raw match
                  case b: Boolean => Right(FieldValue.Flag(b))
                  case other      => Left(EvalError.ShapeMismatch(path, FieldType.Bool, describe(other)))
              case (FieldType.Number, _) =>
                raw match
                  case n: BigDecimal => Right(FieldValue.Num(n))
                  case n: Int        => Right(FieldValue.Num(BigDecimal(n)))
                  case n: Long       => Right(FieldValue.Num(BigDecimal(n)))
                  case other         => Left(EvalError.ShapeMismatch(path, FieldType.Number, describe(other)))
      walk(record, steps)
    }

  // A derived schema always agrees with the record it was derived from, so the
  // ShapeMismatch branches above are only reachable for a hand-written schema.
  // They report rather than throw, because a hand-written schema is a user
  // input like any other.
  private def describe(value: Any): String =
    if value == null then "null" else value.getClass.getSimpleName
