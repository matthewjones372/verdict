package verdict

/** The rule language.
  *
  * There is deliberately no case holding a function. A `Predicate(f: A =>
  * Boolean)` case would be the obvious convenience, and it would make the rest
  * of the program invisible: every interpreter below this file — validation,
  * SQL, `fieldsUsed`, `describe`, `simplify` — can only see what the AST spells
  * out, and a function body is opaque to all of them. Keeping the language
  * fixed-shape is what makes a rule something other than a compiled predicate.
  */
enum Rule:
  case Gt(path: FieldPath, value: BigDecimal)
  case Gte(path: FieldPath, value: BigDecimal)
  case Lt(path: FieldPath, value: BigDecimal)
  case Lte(path: FieldPath, value: BigDecimal)
  case EqStr(path: FieldPath, value: String)
  case In(path: FieldPath, values: Set[String])
  case IsTrue(path: FieldPath)
  case And(left: Rule, right: Rule)
  case Or(left: Rule, right: Rule)
  case Not(inner: Rule)

  def &&(that: Rule): Rule = Rule.And(this, that)
  def ||(that: Rule): Rule = Rule.Or(this, that)
  def unary_! : Rule       = Rule.Not(this)
