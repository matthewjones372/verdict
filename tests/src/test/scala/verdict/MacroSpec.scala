package verdict

import verdict.Fixtures.*
import verdict.example.Trade
import verdict.macros.rule
import zio.test.*

object MacroSpec extends ZIOSpecDefault:

  def spec = suite("rule[A] { ... }")(
    test("a comparison against a numeric literal") {
      assertTrue(rule[Trade](t => t.notional > 1000000) == Rule.Gt(notional, BigDecimal(1000000)))
    },
    test("every comparison operator") {
      assertTrue(
        rule[Trade](t => t.notional >= 10) == Rule.Gte(notional, BigDecimal(10)),
        rule[Trade](t => t.notional < 10) == Rule.Lt(notional, BigDecimal(10)),
        rule[Trade](t => t.notional <= 10) == Rule.Lte(notional, BigDecimal(10))
      )
    },
    test("a decimal literal keeps the digits it was written with") {
      assertTrue(rule[Trade](t => t.notional > 0.005) == Rule.Gt(notional, BigDecimal("0.005")))
    },
    test("a comparison written the other way round flips the operator") {
      assertTrue(rule[Trade](t => 1000000 < t.notional) == Rule.Gt(notional, BigDecimal(1000000)))
    },
    test("an Int field compares as a number") {
      assertTrue(rule[Trade](t => t.counterparty.rating <= 3) == Rule.Lte(rating, BigDecimal(3)))
    },
    test("== on a string") {
      assertTrue(rule[Trade](t => t.currency == "EUR") == Rule.EqStr(currency, "EUR"))
    },
    test("a boolean field on its own") {
      assertTrue(rule[Trade](t => t.isCleared) == Rule.IsTrue(isCleared))
    },
    test("nested field access becomes a nested path") {
      assertTrue(rule[Trade](t => t.counterparty.jurisdiction == "DE") == Rule.EqStr(jurisdiction, "DE"))
    },
    test("Set(...).contains(field) becomes In") {
      assertTrue(
        rule[Trade](t => Set("DE", "FR", "IT").contains(t.counterparty.jurisdiction)) ==
          Rule.In(jurisdiction, Set("DE", "FR", "IT"))
      )
    },
    test("the connectives, with the precedence Scala already gives them") {
      val written = rule[Trade] { t =>
        t.notional > 1000000 && (Set("DE", "FR", "IT").contains(t.counterparty.jurisdiction) || !t.isCleared)
      }
      val built = Rule.And(
        Rule.Gt(notional, BigDecimal(1000000)),
        Rule.Or(Rule.In(jurisdiction, Set("DE", "FR", "IT")), Rule.Not(Rule.IsTrue(isCleared)))
      )
      assertTrue(written == built)
    },
    test("the macro route and the AST route are the same rule, so every interpreter agrees") {
      val written = rule[Trade](t => t.notional > 1000000 && t.counterparty.jurisdiction == "DE")
      assertTrue(
        Validator.validate(written, Schema.of[Trade]).isEmpty,
        Analysis.fieldsUsed(written) == Set(notional, jurisdiction),
        Analysis.describe(written) == """notional is greater than 1000000 and counterparty.jurisdiction is "DE"""",
        SqlInterpreter.toSql(written, Schema.of[Trade], "trades", NamingStrategy.snake).sql ==
          "SELECT * FROM trades WHERE ((notional > ?) AND (counterparty_jurisdiction = ?))",
        Evaluator.holds(written, cleared, Schema.of[Trade]) == Right(true),
        Evaluator.holds(written, small, Schema.of[Trade]) == Right(false)
      )
    },
    suite("what it refuses")(
      test("a misspelt field is a compile error naming the fields that exist") {
        for errors <- typeCheck("""verdict.macros.rule[verdict.example.Trade](t => t.notionel > 1)""")
        yield assertTrue(errors.isLeft, errors.swap.toOption.exists(_.contains("value notionel is not a member")))
      },
      test("a field compared as the wrong type is a compile error") {
        for errors <- typeCheck("""verdict.macros.rule[verdict.example.Trade](t => t.currency == "EUR" && t.isCleared == "yes")""")
        yield assertTrue(errors.isLeft)
      },
      test("a nested field that does not exist is a compile error") {
        for errors <- typeCheck("""verdict.macros.rule[verdict.example.Trade](t => t.counterparty.domicile == "DE")""")
        yield assertTrue(errors.isLeft, errors.swap.toOption.exists(_.contains("domicile")))
      },
      test("comparing a field against something that is not a literal is refused, and says what is allowed") {
        for errors <- typeCheck("""{
          val limit = BigDecimal(10)
          verdict.macros.rule[verdict.example.Trade](t => t.notional > limit)
        }""")
        yield assertTrue(
          errors.isLeft,
          errors.swap.toOption.exists(_.contains("must be a numeric literal")),
          errors.swap.toOption.exists(_.contains("Set(...).contains(field)"))
        )
      },
      test("calling a method of your own inside the lambda is refused") {
        for errors <- typeCheck(
            """verdict.macros.rule[verdict.example.Trade](t => t.currency.startsWith("E"))"""
          )
        yield assertTrue(errors.isLeft, errors.swap.toOption.exists(_.contains("verdict: cannot read")))
      },
      test("contains on something that is not a literal Set is refused") {
        for errors <- typeCheck(
            """verdict.macros.rule[verdict.example.Trade](t => List("DE").contains(t.counterparty.jurisdiction))"""
          )
        yield assertTrue(errors.isLeft, errors.swap.toOption.exists(_.contains("literal Set")))
      },
      test("the schema check catches what the typer lets through, because == accepts anything") {
        // Scala is happy to compare a BigDecimal with a String; the macro is
        // not, because the rule it would build could not evaluate.
        for errors <- typeCheck("""verdict.macros.rule[verdict.example.Trade](t => t.notional == "EUR")""")
        yield assertTrue(
          errors.isLeft,
          errors.swap.toOption.exists(_.contains("notional is a number field, and this compares it as a text"))
        )
      },
      test("a nested record used as a value is refused") {
        for errors <- typeCheck("""verdict.macros.rule[verdict.example.Trade](t => t.counterparty == "x")""")
        yield assertTrue(errors.isLeft)
      }
    )
  )
