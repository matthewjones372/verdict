package verdict

/** The types a rule can compare against directly.
  *
  * Anything without a `Leaf` instance and without a `Mirror.ProductOf` is a
  * compile error during derivation, which is the point: the AST has three leaf
  * shapes, so a schema may not claim a fourth.
  */
trait Leaf[A]:
  def fieldType: FieldType

object Leaf:
  private def instance[A](tpe: FieldType): Leaf[A] = new Leaf[A]:
    def fieldType: FieldType = tpe

  given text: Leaf[String]        = instance(FieldType.Text)
  given decimal: Leaf[BigDecimal] = instance(FieldType.Number)
  given int: Leaf[Int]            = instance(FieldType.Number)
  given long: Leaf[Long]          = instance(FieldType.Number)
  given bool: Leaf[Boolean]       = instance(FieldType.Bool)
