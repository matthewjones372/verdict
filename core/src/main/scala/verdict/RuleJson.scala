package verdict

/** The persistence format for a rule.
  *
  * A stored rule is data that outlives the code that wrote it, so loading one
  * checks it against the current schema before handing it back.
  */
object RuleJson:

  def encode(rule: Rule): Json = rule match
    case Rule.Gt(p, v)  => comparison("gt", p, v)
    case Rule.Gte(p, v) => comparison("gte", p, v)
    case Rule.Lt(p, v)  => comparison("lt", p, v)
    case Rule.Lte(p, v) => comparison("lte", p, v)
    case Rule.EqStr(p, v) =>
      Json.Obj(List("op" -> Json.Str("eq"), "path" -> Json.Str(p.render), "value" -> Json.Str(v)))
    case Rule.In(p, values) =>
      // Sorted so that the same rule stores byte for byte the same way, which
      // matters as soon as a stored rule is diffed or checksummed.
      Json.Obj(
        List(
          "op"     -> Json.Str("in"),
          "path"   -> Json.Str(p.render),
          "values" -> Json.Arr(values.toList.sorted.map(Json.Str.apply))
        )
      )
    case Rule.IsTrue(p) =>
      Json.Obj(List("op" -> Json.Str("isTrue"), "path" -> Json.Str(p.render)))
    case Rule.And(l, r) => connective("and", l, r)
    case Rule.Or(l, r)  => connective("or", l, r)
    case Rule.Not(i)    => Json.Obj(List("op" -> Json.Str("not"), "rule" -> encode(i)))

  def render(rule: Rule): String = encode(rule).render

  def decode(json: Json): Either[String, Rule] =
    for
      op   <- text(json, "op")
      rule <- op match
        case "gt"  => comparisonOf(json, Rule.Gt.apply)
        case "gte" => comparisonOf(json, Rule.Gte.apply)
        case "lt"  => comparisonOf(json, Rule.Lt.apply)
        case "lte" => comparisonOf(json, Rule.Lte.apply)
        case "eq"  => path(json).flatMap(p => text(json, "value").map(Rule.EqStr(p, _)))
        case "in" =>
          for
            p      <- path(json)
            values <- json.field("values").toRight("'values' is missing").flatMap {
              case Json.Arr(items) =>
                items.foldRight[Either[String, List[String]]](Right(Nil)) {
                  case (Json.Str(v), acc) => acc.map(v :: _)
                  case (other, _)         => Left(s"'values' holds ${other.render}, which is not a string")
                }
              case other => Left(s"'values' is ${other.render}, which is not an array")
            }
          yield Rule.In(p, values.toSet)
        case "isTrue" => path(json).map(Rule.IsTrue.apply)
        case "and"    => connectiveOf(json, Rule.And.apply)
        case "or"     => connectiveOf(json, Rule.Or.apply)
        case "not"    => nested(json, "rule").map(Rule.Not.apply)
        case unknown  => Left(s"'$unknown' is not an operator this version understands")
    yield rule

  def parse(document: String): Either[String, Rule] = Json.parse(document).flatMap(decode)

  /** Reads a stored rule and checks it against the schema it will be run with.
    *
    * A stored rule was written against some earlier version of the record. This
    * is where that shows up, rather than as an evaluation error later.
    */
  def load(document: String, schema: Schema[?]): Either[List[ValidationError], Rule] =
    parse(document) match
      case Left(reason) => Left(List(ValidationError.Unreadable(reason)))
      case Right(rule) =>
        Validator.validate(rule, schema) match
          case Nil    => Right(rule)
          case errors => Left(errors)

  private def comparison(op: String, path: FieldPath, value: BigDecimal): Json =
    Json.Obj(List("op" -> Json.Str(op), "path" -> Json.Str(path.render), "value" -> Json.Num(value)))

  private def connective(op: String, left: Rule, right: Rule): Json =
    Json.Obj(List("op" -> Json.Str(op), "left" -> encode(left), "right" -> encode(right)))

  private def text(json: Json, name: String): Either[String, String] =
    json.field(name) match
      case Some(Json.Str(value)) => Right(value)
      case Some(other)           => Left(s"'$name' is ${other.render}, which is not a string")
      case None                  => Left(s"'$name' is missing")

  private def path(json: Json): Either[String, FieldPath] =
    text(json, "path").flatMap(FieldPath.parse)

  private def nested(json: Json, name: String): Either[String, Rule] =
    json.field(name).toRight(s"'$name' is missing").flatMap(decode)

  private def comparisonOf(json: Json, build: (FieldPath, BigDecimal) => Rule): Either[String, Rule] =
    for
      p <- path(json)
      v <- json.field("value") match
        case Some(Json.Num(value)) => Right(value)
        case Some(other)           => Left(s"'value' is ${other.render}, which is not a number")
        case None                  => Left("'value' is missing")
    yield build(p, v)

  private def connectiveOf(json: Json, build: (Rule, Rule) => Rule): Either[String, Rule] =
    for
      left  <- nested(json, "left")
      right <- nested(json, "right")
    yield build(left, right)
