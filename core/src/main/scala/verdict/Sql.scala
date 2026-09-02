package verdict

/** A parameterised statement and the values that go with it, in order.
  *
  * The values are kept apart from the string rather than rendered into it. A
  * rule holds strings that came from a stored rule or a UI, so interpolating
  * them would be an injection, and a `BigDecimal` written into SQL text is at
  * the mercy of whatever `toString` gives.
  */
final case class SqlFragment(sql: String, params: List[FieldValue]):

  /** Values spliced into the string, for a log line or a README. Never send
    * this to a database; that is what `sql` and `params` are for.
    */
  def preview: String =
    params.foldLeft(sql) { (rendered, value) =>
      val literal = value match
        case FieldValue.Text(v) => s"'${v.replace("'", "''")}'"
        case FieldValue.Num(v)  => FieldValue.Num(v).render
        case FieldValue.Flag(v) => v.toString.toUpperCase
      rendered.replaceFirst("\\?", java.util.regex.Matcher.quoteReplacement(literal))
    }

/** How a field path becomes a column name.
  *
  * Nested paths are flattened by joining the segments: `counterparty.rating`
  * becomes `counterparty_rating`. That is a convention about the table, not a
  * discovery about it — it assumes the nested record was stored inline. Reading
  * a nested record out of a second table would mean emitting a join, which
  * needs to know about keys and cardinality and is left as future work.
  */
final case class NamingStrategy(overrides: Map[String, String], separator: String):

  def column(path: FieldPath): String =
    overrides.getOrElse(path.render, path.segments.map(NamingStrategy.snakeCase).mkString(separator))

object NamingStrategy:

  val snake: NamingStrategy = NamingStrategy(Map.empty, "_")

  def withOverrides(overrides: Map[String, String]): NamingStrategy =
    NamingStrategy(overrides, "_")

  def snakeCase(name: String): String =
    name.foldLeft(new StringBuilder) { (out, ch) =>
      if ch.isUpper && out.nonEmpty && out.last != '_' then out += '_' += ch.toLower
      else out += ch.toLower
    }.toString

object SqlInterpreter:

  /** Compiles a rule to `SELECT * FROM table WHERE ...`.
    *
    * The rule is assumed to already fit the schema; `toSqlChecked` is the
    * variant that says so first. The schema is part of this signature because
    * it is what decides whether a compiled rule is meaningful at all, and
    * because the nested-path convention below is a claim about the table that
    * only the schema can eventually check.
    */
  def toSql(rule: Rule, schema: Schema[?], table: String, naming: NamingStrategy): SqlFragment =
    val (predicate, params) = compile(rule, naming)
    SqlFragment(s"SELECT * FROM $table WHERE $predicate", params)

  def toSqlChecked(
      rule: Rule,
      schema: Schema[?],
      table: String,
      naming: NamingStrategy
  ): Either[List[ValidationError], SqlFragment] =
    Validator.validate(rule, schema) match
      case Nil    => Right(toSql(rule, schema, table, naming))
      case errors => Left(errors)

  private def compile(rule: Rule, naming: NamingStrategy): (String, List[FieldValue]) =
    rule match
      case Rule.And(l, r) => binary("AND", l, r, naming)
      case Rule.Or(l, r)  => binary("OR", l, r, naming)
      case Rule.Not(i) =>
        val (inner, params) = compile(i, naming)
        (s"NOT ($inner)", params)

      case Rule.Gt(path, v)  => comparison(path, ">", v, naming)
      case Rule.Gte(path, v) => comparison(path, ">=", v, naming)
      case Rule.Lt(path, v)  => comparison(path, "<", v, naming)
      case Rule.Lte(path, v) => comparison(path, "<=", v, naming)

      case Rule.EqStr(path, v) =>
        (s"(${naming.column(path)} = ?)", List(FieldValue.Text(v)))

      case Rule.In(path, values) =>
        // `IN ()` is a syntax error in most dialects, and an empty set holds for
        // nothing, which is what a contradiction evaluates to.
        if values.isEmpty then ("(1 = 0)", Nil)
        else
          val ordered      = values.toList.sorted
          val placeholders = List.fill(ordered.size)("?").mkString(", ")
          (s"(${naming.column(path)} IN ($placeholders))", ordered.map(FieldValue.Text.apply))

      case Rule.IsTrue(path) =>
        (s"(${naming.column(path)} = ?)", List(FieldValue.Flag(true)))

  private def binary(
      connective: String,
      left: Rule,
      right: Rule,
      naming: NamingStrategy
  ): (String, List[FieldValue]) =
    val (l, lp) = compile(left, naming)
    val (r, rp) = compile(right, naming)
    (s"($l $connective $r)", lp ++ rp)

  private def comparison(
      path: FieldPath,
      operator: String,
      value: BigDecimal,
      naming: NamingStrategy
  ): (String, List[FieldValue]) =
    (s"(${naming.column(path)} $operator ?)", List(FieldValue.Num(value)))
