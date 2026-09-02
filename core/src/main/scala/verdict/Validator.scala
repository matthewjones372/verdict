package verdict

enum ValidationError:
  case BadPath(override val path: FieldPath, error: PathError)
  case WrongType(override val path: FieldPath, operator: String, expected: FieldType, actual: FieldType)

  def path: FieldPath = this match
    case BadPath(p, _)         => p
    case WrongType(p, _, _, _) => p

  def message: String = this match
    case BadPath(path, error) => error.message(path)
    case WrongType(path, operator, expected, actual) =>
      s"${path.render}: $operator needs a ${expected.render} field, but ${path.render} is a ${actual.render}"

object Validator:

  /** Checks a rule against a schema without a record in hand.
    *
    * Every error is returned, not the first: a stored rule is usually being
    * reviewed by someone who would rather see the whole list than fix one
    * problem per round trip.
    */
  def validate(rule: Rule, schema: Schema[?]): List[ValidationError] =
    rule match
      case Rule.And(l, r) => validate(l, schema) ++ validate(r, schema)
      case Rule.Or(l, r)  => validate(l, schema) ++ validate(r, schema)
      case Rule.Not(i)    => validate(i, schema)

      case Rule.Gt(path, _)  => leaf(path, schema, ">", FieldType.Number)
      case Rule.Gte(path, _) => leaf(path, schema, ">=", FieldType.Number)
      case Rule.Lt(path, _)  => leaf(path, schema, "<", FieldType.Number)
      case Rule.Lte(path, _) => leaf(path, schema, "<=", FieldType.Number)

      case Rule.EqStr(path, _) => leaf(path, schema, "==", FieldType.Text)
      case Rule.In(path, _)    => leaf(path, schema, "in", FieldType.Text)
      case Rule.IsTrue(path)   => leaf(path, schema, "is true", FieldType.Bool)

  private def leaf(path: FieldPath, schema: Schema[?], operator: String, expected: FieldType): List[ValidationError] =
    schema.typeAt(path) match
      case Left(error)                  => List(ValidationError.BadPath(path, error))
      case Right(actual) if actual == expected => Nil
      case Right(actual)                => List(ValidationError.WrongType(path, operator, expected, actual))
