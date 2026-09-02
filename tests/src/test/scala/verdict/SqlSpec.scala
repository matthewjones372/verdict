package verdict

import verdict.Fixtures.*
import verdict.example.Trade
import zio.test.*

object SqlSpec extends ZIOSpecDefault:

  private val schema = Schema.of[Trade]

  private def sqlOf(rule: Rule, naming: NamingStrategy = NamingStrategy.snake): SqlFragment =
    SqlInterpreter.toSql(rule, schema, "trades", naming)

  def spec = suite("SqlInterpreter")(
    test("camelCase field names become snake_case columns") {
      assertTrue(
        NamingStrategy.snakeCase("isCleared") == "is_cleared",
        NamingStrategy.snakeCase("notional") == "notional",
        NamingStrategy.snakeCase("ISOCode") == "i_s_o_code"
      )
    },
    test("a comparison binds its value rather than writing it into the string") {
      val fragment = sqlOf(Rule.Gt(notional, BigDecimal("1000000")))
      assertTrue(
        fragment.sql == "SELECT * FROM trades WHERE (notional > ?)",
        fragment.params == List(FieldValue.Num(BigDecimal("1000000")))
      )
    },
    test("a string that would end the literal early is a bind value, not text") {
      val fragment = sqlOf(Rule.EqStr(currency, "'; DROP TABLE trades; --"))
      assertTrue(
        fragment.sql == "SELECT * FROM trades WHERE (currency = ?)",
        fragment.params == List(FieldValue.Text("'; DROP TABLE trades; --")),
        !fragment.sql.contains("DROP")
      )
    },
    test("a nested path is flattened by joining its segments") {
      assertTrue(sqlOf(Rule.EqStr(jurisdiction, "DE")).sql.contains("counterparty_jurisdiction = ?"))
    },
    test("an override wins over the naming convention") {
      val naming = NamingStrategy.withOverrides(Map("counterparty.jurisdiction" -> "cp_juris"))
      assertTrue(sqlOf(Rule.EqStr(jurisdiction, "DE"), naming).sql.contains("cp_juris = ?"))
    },
    test("In becomes one placeholder per value, in the order they are bound") {
      val fragment = sqlOf(Rule.In(jurisdiction, Set("IT", "DE", "FR")))
      assertTrue(
        fragment.sql == "SELECT * FROM trades WHERE (counterparty_jurisdiction IN (?, ?, ?))",
        fragment.params == List(FieldValue.Text("DE"), FieldValue.Text("FR"), FieldValue.Text("IT"))
      )
    },
    test("an empty In becomes a contradiction rather than invalid syntax") {
      val fragment = sqlOf(Rule.In(jurisdiction, Set.empty))
      assertTrue(fragment.sql == "SELECT * FROM trades WHERE (1 = 0)", fragment.params.isEmpty)
    },
    test("IsTrue binds the boolean instead of naming the column alone") {
      val fragment = sqlOf(Rule.IsTrue(isCleared))
      assertTrue(
        fragment.sql == "SELECT * FROM trades WHERE (is_cleared = ?)",
        fragment.params == List(FieldValue.Flag(true))
      )
    },
    test("the connectives parenthesise, and the binds stay in left-to-right order") {
      val rule = Rule.And(
        Rule.Gt(notional, BigDecimal(1000000)),
        Rule.Or(Rule.In(jurisdiction, Set("DE", "FR")), Rule.Not(Rule.IsTrue(isCleared)))
      )
      val fragment = sqlOf(rule)
      assertTrue(
        fragment.sql ==
          "SELECT * FROM trades WHERE ((notional > ?) AND ((counterparty_jurisdiction IN (?, ?)) OR NOT ((is_cleared = ?))))",
        fragment.params == List(
          FieldValue.Num(BigDecimal(1000000)),
          FieldValue.Text("DE"),
          FieldValue.Text("FR"),
          FieldValue.Flag(true)
        )
      )
    },
    test("preview is for reading, and quotes what it splices") {
      val fragment = sqlOf(Rule.And(Rule.EqStr(currency, "O'Hara"), Rule.Gt(notional, BigDecimal("1e3"))))
      assertTrue(fragment.preview == "SELECT * FROM trades WHERE ((currency = 'O''Hara') AND (notional > 1000))")
    },
    test("toSqlChecked refuses a rule the schema does not accept") {
      val result = SqlInterpreter.toSqlChecked(Rule.Gt(currency, BigDecimal(1)), schema, "trades", NamingStrategy.snake)
      assertTrue(result.swap.toOption.map(_.map(_.message)) == Some(List("currency: > needs a number field, but currency is a text")))
    },
    test("toSqlChecked passes a rule the schema does accept") {
      val result = SqlInterpreter.toSqlChecked(Rule.Gt(notional, BigDecimal(1)), schema, "trades", NamingStrategy.snake)
      assertTrue(result.map(_.sql) == Right("SELECT * FROM trades WHERE (notional > ?)"))
    }
  )
