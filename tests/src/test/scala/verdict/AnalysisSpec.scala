package verdict

import verdict.Fixtures.*
import verdict.example.Trade
import zio.test.*

object AnalysisSpec extends ZIOSpecDefault:

  private val schema = Schema.of[Trade]

  private val example = Rule.And(
    Rule.Gt(notional, BigDecimal(1000000)),
    Rule.Or(Rule.In(jurisdiction, Set("DE", "FR", "IT")), Rule.Not(Rule.IsTrue(isCleared)))
  )

  def spec = suite("Analysis")(
    suite("fieldsUsed")(
      test("collects every path the rule reads, once each") {
        assertTrue(Analysis.fieldsUsed(example) == Set(notional, jurisdiction, isCleared))
      },
      test("a path read twice is still one path") {
        val rule = Rule.And(Rule.Gt(notional, BigDecimal(1)), Rule.Lt(notional, BigDecimal(9)))
        assertTrue(Analysis.fieldsUsed(rule) == Set(notional))
      },
      test("every path it names resolves against the schema it validates under") {
        check(RuleGen.rules) { rule =>
          val used = Analysis.fieldsUsed(rule)
          assertTrue(Validator.validate(rule, schema).isEmpty, used.forall(schema.typeAt(_).isRight))
        }
      }
    ),
    suite("describe")(
      test("renders a rule in English") {
        assertTrue(
          Analysis.describe(example) ==
            """notional is greater than 1000000 and (counterparty.jurisdiction is one of "DE", "FR", "IT" or not (isCleared is true))"""
        )
      },
      test("a chain of the same connective is not over-bracketed") {
        val rule = Rule.And(Rule.And(Rule.IsTrue(isCleared), Rule.EqStr(currency, "EUR")), Rule.Gte(rating, BigDecimal(3)))
        assertTrue(Analysis.describe(rule) == """isCleared is true and currency is "EUR" and counterparty.rating is at least 3""")
      },
      test("every operator has words") {
        assertTrue(
          Analysis.describe(Rule.Gt(notional, BigDecimal(1))) == "notional is greater than 1",
          Analysis.describe(Rule.Gte(notional, BigDecimal(1))) == "notional is at least 1",
          Analysis.describe(Rule.Lt(notional, BigDecimal(1))) == "notional is less than 1",
          Analysis.describe(Rule.Lte(notional, BigDecimal(1))) == "notional is at most 1",
          Analysis.describe(Rule.In(currency, Set.empty)) == "currency is one of nothing"
        )
      }
    ),
    suite("simplify")(
      test("removes a double negation") {
        assertTrue(Analysis.simplify(Rule.Not(Rule.Not(Rule.IsTrue(isCleared)))) == Rule.IsTrue(isCleared))
      },
      test("collapses a repeated conjunct") {
        val rule = Rule.And(Rule.IsTrue(isCleared), Rule.IsTrue(isCleared))
        assertTrue(Analysis.simplify(rule) == Rule.IsTrue(isCleared))
      },
      test("keeps the stricter of two lower bounds under an and") {
        val rule = Rule.And(Rule.Gt(notional, BigDecimal(1)), Rule.Gt(notional, BigDecimal(2)))
        assertTrue(Analysis.simplify(rule) == Rule.Gt(notional, BigDecimal(2)))
      },
      test("keeps the looser of two lower bounds under an or") {
        val rule = Rule.Or(Rule.Gt(notional, BigDecimal(1)), Rule.Gt(notional, BigDecimal(2)))
        assertTrue(Analysis.simplify(rule) == Rule.Gt(notional, BigDecimal(1)))
      },
      test("an upper bound tightens downwards") {
        assertTrue(
          Analysis.simplify(Rule.And(Rule.Lt(notional, BigDecimal(9)), Rule.Lte(notional, BigDecimal(2)))) ==
            Rule.Lte(notional, BigDecimal(2)),
          Analysis.simplify(Rule.Or(Rule.Lt(notional, BigDecimal(9)), Rule.Lte(notional, BigDecimal(2)))) ==
            Rule.Lt(notional, BigDecimal(9))
        )
      },
      test("on equal bounds the and takes the exclusive one and the or the inclusive one") {
        assertTrue(
          Analysis.simplify(Rule.And(Rule.Gt(notional, BigDecimal(5)), Rule.Gte(notional, BigDecimal(5)))) ==
            Rule.Gt(notional, BigDecimal(5)),
          Analysis.simplify(Rule.Or(Rule.Gt(notional, BigDecimal(5)), Rule.Gte(notional, BigDecimal(5)))) ==
            Rule.Gte(notional, BigDecimal(5))
        )
      },
      test("two equalities on one path become one membership under an or") {
        val rule = Rule.Or(Rule.EqStr(currency, "EUR"), Rule.EqStr(currency, "USD"))
        assertTrue(Analysis.simplify(rule) == Rule.In(currency, Set("EUR", "USD")))
      },
      test("two memberships on one path intersect under an and, and a single value reads as an equality") {
        val rule = Rule.And(Rule.In(currency, Set("EUR", "USD")), Rule.In(currency, Set("USD", "GBP")))
        assertTrue(Analysis.simplify(rule) == Rule.EqStr(currency, "USD"))
      },
      test("contradictory equalities become a membership of nothing") {
        val rule = Rule.And(Rule.EqStr(currency, "EUR"), Rule.EqStr(currency, "USD"))
        assertTrue(Analysis.simplify(rule) == Rule.In(currency, Set.empty))
      },
      test("constraints on different paths are left alone") {
        val rule = Rule.And(Rule.Gt(notional, BigDecimal(1)), Rule.Gt(rating, BigDecimal(2)))
        assertTrue(Analysis.simplify(rule) == rule)
      },
      test("a conjunct absorbs an alternative that contains it") {
        val rule = Rule.And(Rule.IsTrue(isCleared), Rule.Or(Rule.IsTrue(isCleared), Rule.EqStr(currency, "EUR")))
        assertTrue(Analysis.simplify(rule) == Rule.IsTrue(isCleared))
      },
      test("simplification is idempotent") {
        check(RuleGen.rules) { rule =>
          val once = Analysis.simplify(rule)
          assertTrue(Analysis.simplify(once) == once)
        }
      },
      test("simplification never changes the verdict") {
        // The claim the rewrite makes, checked against the evaluator rather
        // than against a second copy of the rewrite.
        check(RuleGen.rules, RuleGen.trade) { (rule, trade) =>
          val before = Evaluator.holds(rule, trade, schema)
          val after  = Evaluator.holds(Analysis.simplify(rule), trade, schema)
          assertTrue(before.isRight, before == after)
        }
      },
      test("simplification never introduces a path the original did not read") {
        check(RuleGen.rules) { rule =>
          assertTrue(Analysis.fieldsUsed(Analysis.simplify(rule)).subsetOf(Analysis.fieldsUsed(rule)))
        }
      },
      test("a simplified rule still validates") {
        check(RuleGen.rules) { rule =>
          assertTrue(Validator.validate(Analysis.simplify(rule), schema).isEmpty)
        }
      }
    )
  )
