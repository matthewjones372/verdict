package verdict

/** A leaf value read out of a record.
  *
  * Numbers are widened to `BigDecimal` on the way in so that a rule written
  * against an `Int` field and one written against a `BigDecimal` field compare
  * the same way, and so that the SQL interpreter has one numeric bind type.
  */
enum FieldValue:
  case Text(value: String)
  case Num(value: BigDecimal)
  case Flag(value: Boolean)

  def render: String = this match
    case Text(v) => v
    case Num(v)  => v.bigDecimal.stripTrailingZeros.toPlainString
    case Flag(v) => v.toString

  def fieldType: FieldType = this match
    case Text(_) => FieldType.Text
    case Num(_)  => FieldType.Number
    case Flag(_) => FieldType.Bool
