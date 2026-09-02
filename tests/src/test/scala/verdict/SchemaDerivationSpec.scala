package verdict

import verdict.example.*
import zio.test.*

object SchemaDerivationSpec extends ZIOSpecDefault:

  def spec = suite("Schema derivation")(
    test("the derived schema is the one that was written by hand") {
      // The hand-written fixture is the thing being retired. If the two ever
      // stop agreeing, one of them is wrong about the record.
      assertTrue(Schema.of[Trade] == Fixtures.tradeSchema)
    },
    test("field order follows the constructor, which is what productElement uses") {
      assertTrue(
        Schema.of[Trade].fieldNames == List("id", "notional", "currency", "isCleared", "counterparty")
      )
    },
    test("leaf types come from the Leaf instances") {
      val fields = Schema.of[Trade].fields.map(f => f.name -> f.tpe).toMap
      assertTrue(
        fields("id") == FieldType.Text,
        fields("notional") == FieldType.Number,
        fields("isCleared") == FieldType.Bool
      )
    },
    test("an Int field is a number, so a rule written against it compares numerically") {
      assertTrue(Schema.of[Counterparty].field("rating").map(_.tpe) == Some(FieldType.Number))
    },
    test("a nested case class recurses into a nested schema") {
      val nested = Schema.of[Trade].field("counterparty").map(_.tpe)
      assertTrue(nested == Some(FieldType.Nested(Schema.of[Counterparty])))
    },
    test("a derived schema resolves a nested path") {
      assertTrue(Schema.of[Trade].typeAt(Fixtures.jurisdiction) == Right(FieldType.Text))
    },
    test("Mirror alone carries no annotations") {
      assertTrue(Schema.of[Trade].fields.forall(_.annotations.isEmpty))
    },
    test("the macro reads an annotation off the constructor parameter") {
      assertTrue(Trade.schema.field("id").map(_.annotations) == Some(Set(FieldTag.Identity)))
    },
    test("the macro reads annotations inside a nested record too") {
      val nested = Trade.schema.field("counterparty").map(_.tpe).collect { case FieldType.Nested(s) => s }
      assertTrue(nested.flatMap(_.field("name")).map(_.annotations) == Some(Set(FieldTag.Sensitive)))
    },
    test("an unannotated field gets an empty set rather than a missing entry") {
      assertTrue(Trade.schema.field("notional").map(_.annotations) == Some(Set.empty[FieldTag]))
    },
    test("tagging changes nothing but the tags") {
      assertTrue(Trade.schema.fieldNames == Schema.of[Trade].fieldNames)
    },
    test("the rendered schema shows nesting and tags") {
      assertTrue(
        Trade.schema.render ==
          """Trade
            |  id: text @Identity
            |  notional: number
            |  currency: text
            |  isCleared: boolean
            |  counterparty: Counterparty
            |    name: text @Sensitive
            |    jurisdiction: text
            |    isSanctioned: boolean
            |    rating: number""".stripMargin
      )
    }
  )
