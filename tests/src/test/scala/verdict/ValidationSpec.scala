package verdict

import verdict.Fixtures.*
import verdict.example.*
import zio.test.*

object ValidationSpec extends ZIOSpecDefault:

  /** The same record as `Trade`, after somebody renamed one field. */
  final case class RenamedTrade(
      id: String,
      amount: BigDecimal,
      currency: String,
      isCleared: Boolean,
      counterparty: Counterparty
  ) derives Schema

  private val notionalOverAMillion = Rule.Gt(notional, BigDecimal(1000000))

  def spec = suite("Validator")(
    test("a rule that fits the schema has nothing to report") {
      val rule = Rule.And(notionalOverAMillion, Rule.In(jurisdiction, Set("DE", "FR")))
      assertTrue(Validator.validate(rule, Schema.of[Trade]).isEmpty)
    },
    test("renaming a field in the case class invalidates the rules written against it") {
      // This is what phases 2 and 3 exist for. `notional` became `amount`, and
      // nothing about the rule changed; the schema did, and it says so.
      val errors = Validator.validate(notionalOverAMillion, Schema.of[RenamedTrade])
      assertTrue(
        Validator.validate(notionalOverAMillion, Schema.of[Trade]).isEmpty,
        errors.length == 1,
        errors.head.message.contains("no field 'notional' on RenamedTrade"),
        errors.head.message.contains("amount")
      )
    },
    test("an unknown path names the fields that do exist") {
      val errors = Validator.validate(Rule.EqStr(path("currancy"), "EUR"), Schema.of[Trade])
      assertTrue(
        errors.head.message ==
          "currancy: no field 'currancy' on Trade; available fields are id, notional, currency, isCleared, counterparty"
      )
    },
    test("an unknown segment inside a nested path names the nested type's fields") {
      val errors = Validator.validate(Rule.EqStr(path("counterparty.domicile"), "DE"), Schema.of[Trade])
      assertTrue(
        errors.head.message ==
          "counterparty.domicile: no field 'domicile' on Counterparty; available fields are name, jurisdiction, isSanctioned, rating"
      )
    },
    test("a numeric comparison on a text field is rejected") {
      val errors = Validator.validate(Rule.Gt(currency, BigDecimal(3)), Schema.of[Trade])
      assertTrue(errors.map(_.message) == List("currency: > needs a number field, but currency is a text"))
    },
    test("a string comparison on a boolean field is rejected") {
      val errors = Validator.validate(Rule.EqStr(isCleared, "true"), Schema.of[Trade])
      assertTrue(errors.map(_.message) == List("isCleared: == needs a text field, but isCleared is a boolean"))
    },
    test("In on a number field is rejected, because In compares text") {
      val errors = Validator.validate(Rule.In(rating, Set("1", "2")), Schema.of[Trade])
      assertTrue(errors.length == 1, errors.head.message.contains("in needs a text field"))
    },
    test("a nested record used as a leaf is rejected") {
      val errors = Validator.validate(Rule.EqStr(path("counterparty"), "x"), Schema.of[Trade])
      assertTrue(errors.head.message == "counterparty: == needs a text field, but counterparty is a Counterparty")
    },
    test("walking into a leaf as if it were a record is rejected") {
      val errors = Validator.validate(Rule.EqStr(path("currency.iso"), "EUR"), Schema.of[Trade])
      assertTrue(errors.head.message.contains("'currency' is a text, so it has no fields to walk into"))
    },
    test("every error is reported, not just the first") {
      val rule = Rule.And(
        Rule.Or(Rule.Gt(currency, BigDecimal(1)), Rule.EqStr(path("nope"), "x")),
        Rule.Not(Rule.IsTrue(notional))
      )
      val errors = Validator.validate(rule, Schema.of[Trade])
      assertTrue(
        errors.length == 3,
        errors.flatMap(_.at).map(_.render) == List("currency", "nope", "notional")
      )
    },
    test("a rule that fails validation also fails to evaluate, on the same path") {
      val errors = Validator.validate(Rule.Gt(currency, BigDecimal(1)), Schema.of[Trade])
      val result = Evaluator.evaluate(Rule.Gt(currency, BigDecimal(1)), cleared, Schema.of[Trade])
      assertTrue(
        errors.length == 1,
        result.swap.toOption.map(_.message) == Some(errors.head.message)
      )
    }
  )
