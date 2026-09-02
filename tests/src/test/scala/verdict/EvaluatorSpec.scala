package verdict

import verdict.Fixtures.*
import zio.test.*

object EvaluatorSpec extends ZIOSpecDefault:

  private def evidenceOf(rule: Rule, trade: example.Trade): Evidence =
    Evaluator.evaluate(rule, trade, tradeSchema).fold(e => sys.error(e.message), identity)

  def spec = suite("Evaluator")(
    test("a comparison leaf names the path, the actual value, the operator and the expectation") {
      val evidence = evidenceOf(Rule.Gt(notional, BigDecimal(1000000)), cleared)
      assertTrue(
        evidence == Evidence.Leaf("notional (2500000) > 1000000", true),
        evidence.held
      )
    },
    test("a failing leaf still reports the actual value") {
      val evidence = evidenceOf(Rule.Gt(notional, BigDecimal(1000000)), small)
      assertTrue(
        evidence == Evidence.Leaf("notional (125.5) > 1000000", false),
        !evidence.held
      )
    },
    test("an Int field is compared as a number") {
      assertTrue(
        Evaluator.holds(Rule.Lte(rating, BigDecimal(3)), cleared, tradeSchema) == Right(true),
        Evaluator.holds(Rule.Lte(rating, BigDecimal(3)), small, tradeSchema) == Right(false)
      )
    },
    test("a nested path is walked through the nested schema") {
      val evidence = evidenceOf(Rule.EqStr(jurisdiction, "DE"), cleared)
      assertTrue(evidence == Evidence.Leaf("counterparty.jurisdiction (DE) == DE", true))
    },
    test("In renders the expected set in a stable order") {
      val evidence = evidenceOf(Rule.In(jurisdiction, Set("IT", "DE", "FR")), cleared)
      assertTrue(evidence == Evidence.Leaf("counterparty.jurisdiction (DE) in {DE, FR, IT}", true))
    },
    test("an empty In holds for nothing") {
      assertTrue(Evaluator.holds(Rule.In(jurisdiction, Set.empty), cleared, tradeSchema) == Right(false))
    },
    test("IsTrue reads a boolean field") {
      assertTrue(
        Evaluator.holds(Rule.IsTrue(isCleared), cleared, tradeSchema) == Right(true),
        Evaluator.holds(Rule.IsTrue(isCleared), small, tradeSchema) == Right(false)
      )
    },
    test("And collects both sides even when the first has already failed") {
      val rule     = Rule.And(Rule.Gt(notional, BigDecimal(1000000)), Rule.EqStr(currency, "USD"))
      val evidence = evidenceOf(rule, small)
      assertTrue(
        evidence == Evidence.All(
          List(
            Evidence.Leaf("notional (125.5) > 1000000", false),
            Evidence.Leaf("currency (USD) == USD", true)
          )
        ),
        !evidence.held
      )
    },
    test("a chain of Ands reads as one list rather than a leaning tree") {
      val rule = Rule.And(
        Rule.And(Rule.Gt(notional, BigDecimal(1)), Rule.EqStr(currency, "EUR")),
        Rule.IsTrue(isCleared)
      )
      assertTrue(evidenceOf(rule, cleared).leaves.length == 3) &&
      assert(evidenceOf(rule, cleared))(Assertion.isSubtype[Evidence.All](Assertion.anything)) &&
      assertTrue(evidenceOf(rule, cleared).render() == """PASS all of:
                                                        |  PASS notional (2500000) > 1
                                                        |  PASS currency (EUR) == EUR
                                                        |  PASS isCleared (true) is true""".stripMargin)
    },
    test("an Or nested inside an And keeps its own node") {
      val rule     = Rule.And(Rule.IsTrue(isCleared), Rule.Or(Rule.EqStr(currency, "USD"), Rule.EqStr(currency, "EUR")))
      val evidence = evidenceOf(rule, cleared)
      assertTrue(
        evidence == Evidence.All(
          List(
            Evidence.Leaf("isCleared (true) is true", true),
            Evidence.Any(
              List(
                Evidence.Leaf("currency (EUR) == USD", false),
                Evidence.Leaf("currency (EUR) == EUR", true)
              )
            )
          )
        )
      )
    },
    test("Not wraps rather than rewrites, so the reason survives the negation") {
      val evidence = evidenceOf(Rule.Not(Rule.IsTrue(sanctioned)), sanctionedTrade)
      assertTrue(
        evidence == Evidence.Negation(Evidence.Leaf("counterparty.isSanctioned (true) is true", true)),
        !evidence.held,
        evidence.render() == """FAIL not:
                               |  PASS counterparty.isSanctioned (true) is true""".stripMargin
      )
    },
    test("holds is the evidence's own verdict, for every rule and record") {
      val rules = List(
        Rule.Gt(notional, BigDecimal(1000)),
        Rule.Not(Rule.IsTrue(isCleared)),
        Rule.Or(Rule.In(jurisdiction, Set("DE", "RU")), Rule.Lt(rating, BigDecimal(2))),
        Rule.And(Rule.IsTrue(sanctioned), Rule.EqStr(currency, "GBP"))
      )
      val trades = List(cleared, small, sanctionedTrade)
      assertTrue(
        (for
          rule  <- rules
          trade <- trades
        yield Evaluator.holds(rule, trade, tradeSchema) == Evaluator.evaluate(rule, trade, tradeSchema).map(_.held))
          .forall(identity)
      )
    },
    test("an unknown path is an error that lists the fields that do exist") {
      val result = Evaluator.evaluate(Rule.Gt(path("notionel"), BigDecimal(1)), cleared, tradeSchema)
      assertTrue(
        result.isLeft,
        result.swap.toOption.get.message.contains("no field 'notionel' on Trade"),
        result.swap.toOption.get.message.contains("counterparty")
      )
    },
    test("comparing a text field as a number is an error, not a false") {
      val result = Evaluator.evaluate(Rule.Gt(currency, BigDecimal(1)), cleared, tradeSchema)
      assertTrue(
        result.isLeft,
        result.swap.toOption.get.message == "currency: > needs a number field, but currency is a text"
      )
    },
    test("walking into a leaf as though it were a record is an error") {
      val result = Evaluator.evaluate(Rule.EqStr(path("currency.code"), "EUR"), cleared, tradeSchema)
      assertTrue(result.swap.toOption.get.message.contains("'currency' is a text, so it has no fields"))
    }
  )
