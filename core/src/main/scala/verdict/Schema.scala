package verdict

/** What a record's fields are called and what shape they have. */
enum FieldType:
  case Text, Number, Bool
  case Nested(schema: Schema[?])

  def render: String = this match
    case Text         => "text"
    case Number       => "number"
    case Bool         => "boolean"
    case Nested(s)    => s.typeName

enum FieldTag:
  case Sensitive, Identity

final case class Field(name: String, tpe: FieldType, annotations: Set[FieldTag])

/** A description of the record type `A`.
  *
  * `A` is a phantom parameter: nothing in the schema holds an `A`. It is here
  * so that `derives Schema` is expressible — a `derives` clause needs a type
  * constructor — and so that a schema cannot be paired with a record it does
  * not describe. Interpreters that do not care which record they are looking at
  * take a `Schema[?]`.
  */
final case class Schema[A](typeName: String, fields: List[Field]):

  def fieldNames: List[String] = fields.map(_.name)

  def field(name: String): Option[Field] = fields.find(_.name == name)

  /** The chain of (product element index, field) leading to `path`.
    *
    * The index is the position in the case class's parameter list, which is
    * exactly the position `Product.productElement` uses, because the schema was
    * derived from the same `Mirror` that fixes that order.
    */
  def resolve(path: FieldPath): Either[PathError, List[(Int, Field)]] =
    def go(schema: Schema[?], segments: List[String], acc: List[(Int, Field)]): Either[PathError, List[(Int, Field)]] =
      segments match
        case Nil => Right(acc.reverse)
        case name :: rest =>
          schema.fields.indexWhere(_.name == name) match
            case -1 =>
              Left(PathError.Unknown(name, schema.typeName, schema.fieldNames))
            case idx =>
              val field = schema.fields(idx)
              val step  = (idx, field) :: acc
              (field.tpe, rest) match
                case (_, Nil)                    => Right(step.reverse)
                case (FieldType.Nested(inner), _) => go(inner, rest, step)
                case (leaf, _)                   => Left(PathError.NotARecord(name, leaf))
    go(this, path.segments, Nil)

  def typeAt(path: FieldPath): Either[PathError, FieldType] =
    resolve(path).map(_.last._2.tpe)

  /** Attaches tags to the fields named by dotted path.
    *
    * The tags themselves come from the macros module, because `Mirror` does not
    * carry annotations; this method is the seam between the two.
    */
  def tagged(tags: Map[String, Set[FieldTag]]): Schema[A] =
    def go[B](schema: Schema[B], prefix: String): Schema[B] =
      Schema[B](
        schema.typeName,
        schema.fields.map { f =>
          val path  = if prefix.isEmpty then f.name else s"$prefix.${f.name}"
          val inner = f.tpe match
            case FieldType.Nested(s) => FieldType.Nested(go(s, path))
            case other               => other
          Field(f.name, inner, f.annotations ++ tags.getOrElse(path, Set.empty))
        }
      )
    go(this, "")

  def render: String =
    def go(schema: Schema[?], indent: String): String =
      schema.fields
        .map { f =>
          val tags = if f.annotations.isEmpty then "" else f.annotations.toList.map(_.toString).sorted.mkString(" @", " @", "")
          f.tpe match
            case FieldType.Nested(inner) =>
              s"$indent${f.name}: ${inner.typeName}$tags\n${go(inner, indent + "  ")}"
            case leaf =>
              s"$indent${f.name}: ${leaf.render}$tags"
        }
        .mkString("\n")
    s"$typeName\n${go(this, "  ")}"

enum PathError:
  case Unknown(segment: String, in: String, available: List[String])
  case NotARecord(segment: String, tpe: FieldType)

  def message(path: FieldPath): String = this match
    case Unknown(segment, in, available) =>
      s"${path.render}: no field '$segment' on $in; available fields are ${available.mkString(", ")}"
    case NotARecord(segment, tpe) =>
      s"${path.render}: '$segment' is a ${tpe.render}, so it has no fields to walk into"
