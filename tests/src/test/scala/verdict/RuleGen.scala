package verdict

import verdict.example.*
import zio.test.*

/** Rules and records that fit `Trade`.
  *
  * The value pools overlap on purpose: a generator that never produces a
  * currency a rule asks about would make every property pass for the wrong
  * reason.
  */
object RuleGen:

  val currencies   = List("EUR", "USD", "GBP", "JPY")
  val jurisdictions = List("DE", "FR", "IT", "US", "RU")
  val names        = List("Banco Uno", "Sunrise LLC", "Opaque Holdings")

  private val numericPaths = List(Fixtures.notional, Fixtures.rating)
  private val textPaths    = List(Fixtures.currency, Fixtures.jurisdiction)
  private val boolPaths    = List(Fixtures.isCleared, Fixtures.sanctioned)

  val counterparty: Gen[Any, Counterparty] =
    for
      name         <- Gen.elements(names*)
      jurisdiction <- Gen.elements(jurisdictions*)
      sanctioned   <- Gen.boolean
      rating       <- Gen.int(1, 10)
    yield Counterparty(name, jurisdiction, sanctioned, rating)

  val trade: Gen[Any, Trade] =
    for
      id       <- Gen.alphaNumericStringBounded(4, 8)
      notional <- Gen.bigDecimal(BigDecimal(0), BigDecimal(5000000))
      currency <- Gen.elements(currencies*)
      cleared  <- Gen.boolean
      cp       <- counterparty
    yield Trade(id, notional, currency, cleared, cp)

  private val bound: Gen[Any, BigDecimal] =
    Gen.elements(BigDecimal(0), BigDecimal(1), BigDecimal(3), BigDecimal(5), BigDecimal(1000000), BigDecimal(5000000))

  val leaf: Gen[Any, Rule] =
    Gen.oneOf(
      Gen.elements(numericPaths*).zip(bound).map(Rule.Gt.apply),
      Gen.elements(numericPaths*).zip(bound).map(Rule.Gte.apply),
      Gen.elements(numericPaths*).zip(bound).map(Rule.Lt.apply),
      Gen.elements(numericPaths*).zip(bound).map(Rule.Lte.apply),
      Gen.elements(textPaths*).zip(Gen.elements((currencies ++ jurisdictions)*)).map(Rule.EqStr.apply),
      Gen
        .elements(textPaths*)
        .zip(Gen.setOfBounded(0, 3)(Gen.elements((currencies ++ jurisdictions)*)))
        .map(Rule.In.apply),
      Gen.elements(boolPaths*).map(Rule.IsTrue.apply)
    )

  def rule(depth: Int): Gen[Any, Rule] =
    if depth <= 0 then leaf
    else
      Gen.oneOf(
        leaf,
        leaf,
        Gen.suspend(rule(depth - 1).zip(rule(depth - 1)).map(Rule.And.apply)),
        Gen.suspend(rule(depth - 1).zip(rule(depth - 1)).map(Rule.Or.apply)),
        Gen.suspend(rule(depth - 1).map(Rule.Not.apply))
      )

  val rules: Gen[Any, Rule] = rule(3)
