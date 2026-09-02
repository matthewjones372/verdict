package verdict

import verdict.Fixtures.*
import verdict.example.Trade
import zio.test.*

object JsonSpec extends ZIOSpecDefault:

  private val schema = Schema.of[Trade]

  private val example = Rule.And(
    Rule.Gt(notional, BigDecimal(1000000)),
    Rule.Or(Rule.In(jurisdiction, Set("DE", "FR")), Rule.Not(Rule.IsTrue(isCleared)))
  )

  def spec = suite("RuleJson")(
    suite("the JSON itself")(
      test("parses the shapes it emits") {
        assertTrue(
          Json.parse("""{"a":[1,-2.5,1e3],"b":null,"c":{"d":true}}""").map(_.render) ==
            Right("""{"a":[1,-2.5,1000],"b":null,"c":{"d":true}}""")
        )
      },
      test("round-trips a string through its escapes") {
        val awkward = "quote \" backslash \\ newline \n tab \t control  unicode é"
        assertTrue(Json.parse(Json.Str(awkward).render) == Right(Json.Str(awkward)))
      },
      test("reports where a document goes wrong") {
        assertTrue(
          Json.parse("""{"op":}""").isLeft,
          Json.parse("""{"op":"gt"} trailing""").swap.toOption.exists(_.contains("unexpected content")),
          Json.parse("{\"op\":\"gt").isLeft,
          Json.parse("").isLeft
        )
      }
    ),
    suite("codec")(
      test("a rule renders as the document that describes it") {
        assertTrue(
          RuleJson.render(example) ==
            """{"op":"and","left":{"op":"gt","path":"notional","value":1000000},"right":{"op":"or","left":{"op":"in","path":"counterparty.jurisdiction","values":["DE","FR"]},"right":{"op":"not","rule":{"op":"isTrue","path":"isCleared"}}}}"""
        )
      },
      test("a set stores in a stable order, so the same rule stores the same bytes") {
        assertTrue(
          RuleJson.render(Rule.In(currency, Set("USD", "EUR", "GBP"))) ==
            RuleJson.render(Rule.In(currency, Set("GBP", "USD", "EUR")))
        )
      },
      test("round-trips every rule the generator can build") {
        check(RuleGen.rules) { rule =>
          assertTrue(RuleJson.parse(RuleJson.render(rule)) == Right(rule))
        }
      },
      test("a decimal keeps its value through the round trip") {
        val rule = Rule.Gt(notional, BigDecimal("0.000000000000000000001"))
        assertTrue(RuleJson.parse(RuleJson.render(rule)) == Right(rule))
      },
      test("an operator from a newer version is refused by name") {
        assertTrue(
          RuleJson.parse("""{"op":"between","path":"notional"}""") ==
            Left("'between' is not an operator this version understands")
        )
      },
      test("a missing or mistyped member is refused, and says which") {
        assertTrue(
          RuleJson.parse("""{"op":"gt","path":"notional"}""") == Left("'value' is missing"),
          RuleJson.parse("""{"op":"gt","path":"notional","value":"big"}""") ==
            Left("""'value' is "big", which is not a number"""),
          RuleJson.parse("""{"op":"in","path":"notional","values":[1]}""") ==
            Left("'values' holds 1, which is not a string"),
          RuleJson.parse("""{"op":"and","left":{"op":"isTrue","path":"isCleared"}}""") == Left("'right' is missing")
        )
      },
      test("a stored path that is not a path is refused before it reaches the schema") {
        assertTrue(RuleJson.parse("""{"op":"isTrue","path":"is cleared"}""").isLeft)
      }
    ),
    suite("load")(
      test("a stored rule that fits the current schema comes back") {
        assertTrue(RuleJson.load(RuleJson.render(example), schema) == Right(example))
      },
      test("a stored rule written against an older record fails against the current one") {
        val stored = """{"op":"gt","path":"notionalAmount","value":1000000}"""
        val errors = RuleJson.load(stored, schema).swap.toOption.get
        assertTrue(errors.map(_.message).head.contains("no field 'notionalAmount' on Trade"))
      },
      test("a document that is not a rule fails as a validation error rather than an exception") {
        val errors = RuleJson.load("{ not json", schema).swap.toOption.get
        assertTrue(
          errors.length == 1,
          errors.head.at.isEmpty,
          errors.head.message.startsWith("this is not a stored rule:")
        )
      },
      test("a loaded rule evaluates exactly as the rule that was stored") {
        check(RuleGen.rules, RuleGen.trade) { (rule, trade) =>
          val loaded = RuleJson.load(RuleJson.render(rule), schema)
          assertTrue(
            loaded == Right(rule),
            loaded.map(Evaluator.evaluate(_, trade, schema)) == Right(Evaluator.evaluate(rule, trade, schema))
          )
        }
      }
    )
  )
