package verdict

import verdict.Fixtures.*
import verdict.example.{Counterparty, Trade}

import java.sql.Connection
import scala.math.BigDecimal.RoundingMode
import zio.*
import zio.test.*
import zio.test.magnolia.DeriveGen

/** The claim the rest of the library makes is that the explainable path and the
  * fast path cannot disagree. Everything else in the repo is an argument for
  * that; this is the part that checks it.
  */
object DifferentialSpec extends ZIOSpecDefault:

  private val schema = Schema.of[Trade]
  private val naming = NamingStrategy.snake

  // A small pool, so that a rule asking for jurisdiction "DE" actually matches
  // something. Random strings would make every rule select nothing, and the two
  // sides would agree for the wrong reason.
  private val words = List("DE", "FR", "IT", "US", "RU", "EUR", "USD", "GBP", "JPY", "Banco", "Sunrise", "Opaque")

  // These narrow the leaves before magnolia assembles the record. NUMERIC(20,4)
  // is what the column holds, so a value with more scale than that would come
  // back changed and the two sides would differ over the database's rounding
  // rather than over the rule.
  private given DeriveGen[String] = DeriveGen.instance(Gen.elements(words*))
  private given DeriveGen[Int]    = DeriveGen.instance(Gen.int(1, 10))
  private given DeriveGen[BigDecimal] = DeriveGen.instance(
    // Half the values are drawn from the constants the rules below compare
    // against. A purely random decimal never lands exactly on a boundary, so
    // without this the difference between > and >= would go unnoticed.
    Gen.oneOf(
      Gen.elements(BigDecimal(0), BigDecimal(1000000), BigDecimal("2500000.0000"), BigDecimal(5000000)),
      Gen.bigDecimal(BigDecimal(0), BigDecimal(5000000))
    ).map(_.setScale(4, RoundingMode.HALF_UP))
  )

  // The same mechanism as phase 2: magnolia reads the case class definition and
  // assembles a generator field by field, exactly as `Schema.derived` assembles
  // a field list. Neither one is maintained by hand, and both go stale in the
  // same way when the record changes, which is the point.
  private val generatedTrade: Gen[Any, Trade] = DeriveGen[Trade]

  private val trades: Gen[Any, List[Trade]] =
    Gen.listOfBounded(0, 12)(generatedTrade).map { generated =>
      // Ids come from the pool too, so they collide; the primary key does not
      // care about the shrinker's opinion of a string.
      generated.zipWithIndex.map((trade, index) => trade.copy(id = f"T-$index%03d"))
    }

  private val rules: List[Rule] = List(
    Rule.Gt(notional, BigDecimal(1000000)),
    Rule.Gte(notional, BigDecimal("2500000.0000")),
    Rule.Lt(rating, BigDecimal(5)),
    Rule.Lte(rating, BigDecimal(5)),
    Rule.EqStr(currency, "EUR"),
    Rule.In(jurisdiction, Set("DE", "FR", "IT")),
    Rule.In(jurisdiction, Set.empty),
    Rule.IsTrue(isCleared),
    Rule.Not(Rule.IsTrue(sanctioned)),
    Rule.And(Rule.Gt(notional, BigDecimal(1000000)), Rule.IsTrue(isCleared)),
    Rule.Or(Rule.In(jurisdiction, Set("DE", "RU")), Rule.Not(Rule.IsTrue(isCleared))),
    Rule.And(
      Rule.Gt(notional, BigDecimal(1000000)),
      Rule.Or(Rule.In(jurisdiction, Set("DE", "FR", "IT")), Rule.Not(Rule.IsTrue(isCleared)))
    ),
    Rule.Not(
      Rule.And(Rule.Lte(notional, BigDecimal(3000000)), Rule.Or(Rule.EqStr(currency, "USD"), Rule.IsTrue(sanctioned)))
    )
  )

  private def inMemory(rule: Rule, trades: List[Trade]): Set[String] =
    trades.filter(trade => Evaluator.holds(rule, trade, schema).getOrElse(false)).map(_.id).toSet

  private def compare(connection: Connection, rule: Rule, sample: List[Trade]): Task[TestResult] =
    val fragment = SqlInterpreter.toSql(rule, schema, "trades", naming)
    for
      fromSql <- H2.select(connection, fragment)
      expected = inMemory(rule, sample)
    yield assertTrue(fromSql == expected) ?? s"${Analysis.describe(rule)}  |  ${fragment.preview}"

  def spec = suite("differential: the evaluator and the generated SQL")(
    test("select the same records, for every rule, over random trades") {
      check(trades) { sample =>
        for
          connection <- ZIO.service[Connection]
          _          <- H2.replaceAll(connection, sample)
          results    <- ZIO.foreach(rules)(compare(connection, _, sample))
        yield results.reduce(_ && _)
      }
    },
    test("and again rule by rule, over a fixed awkward set of trades") {
      val sample = List(
        Trade("T-000", BigDecimal("1000000.0000"), "EUR", true, Counterparty("Banco", "DE", false, 5)),
        Trade("T-001", BigDecimal("2500000.0000"), "USD", false, Counterparty("Opaque", "RU", true, 1)),
        Trade("T-002", BigDecimal("0.0000"), "GBP", false, Counterparty("Sunrise", "US", false, 10))
      )
      for
        connection <- ZIO.service[Connection]
        _          <- H2.replaceAll(connection, sample)
        result     <- checkAll(Gen.fromIterable(rules))(compare(connection, _, sample))
      yield result
    },
    test("a simplified rule selects what the original selected") {
      check(trades, RuleGen.rules) { (sample, rule) =>
        val original   = SqlInterpreter.toSql(rule, schema, "trades", naming)
        val simplified = SqlInterpreter.toSql(Analysis.simplify(rule), schema, "trades", naming)
        for
          connection <- ZIO.service[Connection]
          _          <- H2.replaceAll(connection, sample)
          a          <- H2.select(connection, original)
          b          <- H2.select(connection, simplified)
        yield assertTrue(a == b, a == inMemory(rule, sample))
      }
    },
    test("shrinking narrows a failing list down to one trade") {
      // Worth checking rather than assuming: a failure report is only useful if
      // the counterexample is small, and shrinking a derived generator is not
      // something this repo controls.
      val offending: List[Trade] => Boolean = _.exists(_.notional > BigDecimal(4000000))
      for
        first   <- trades.sample.filter(candidate => offending(candidate.value)).runHead
        sample  <- ZIO.fromOption(first).orElseFail(new AssertionError("no failing sample was generated"))
        minimal <- sample.shrinkSearch(offending).filter(offending).runLast
      yield assertTrue(minimal.map(_.length) == Some(1))
    }
  ).provideShared(H2.layer) @@ TestAspect.sequential @@ TestAspect.samples(40)
