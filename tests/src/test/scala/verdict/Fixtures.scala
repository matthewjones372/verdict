package verdict

import verdict.example.*

/** Schemas written out by hand.
  *
  * Phase 2 derives these instead; the hand-written pair stays because the
  * evaluator has to work against a schema it did not derive, and because the
  * derivation test needs something to compare against.
  */
object Fixtures:

  val counterpartySchema: Schema[Counterparty] =
    Schema[Counterparty](
      "Counterparty",
      List(
        Field("name", FieldType.Text, Set.empty),
        Field("jurisdiction", FieldType.Text, Set.empty),
        Field("isSanctioned", FieldType.Bool, Set.empty),
        Field("rating", FieldType.Number, Set.empty)
      )
    )

  val tradeSchema: Schema[Trade] =
    Schema[Trade](
      "Trade",
      List(
        Field("id", FieldType.Text, Set.empty),
        Field("notional", FieldType.Number, Set.empty),
        Field("currency", FieldType.Text, Set.empty),
        Field("isCleared", FieldType.Bool, Set.empty),
        Field("counterparty", FieldType.Nested(counterpartySchema), Set.empty)
      )
    )

  def path(raw: String): FieldPath =
    FieldPath.parse(raw).getOrElse(sys.error(s"fixture path '$raw' is malformed"))

  val notional     = path("notional")
  val currency     = path("currency")
  val isCleared    = path("isCleared")
  val jurisdiction = path("counterparty.jurisdiction")
  val sanctioned   = path("counterparty.isSanctioned")
  val rating       = path("counterparty.rating")

  val cleared = Trade(
    id = "T-1",
    notional = BigDecimal("2500000"),
    currency = "EUR",
    isCleared = true,
    counterparty = Counterparty("Banco Uno", "DE", isSanctioned = false, rating = 3)
  )

  val small = Trade(
    id = "T-2",
    notional = BigDecimal("125.50"),
    currency = "USD",
    isCleared = false,
    counterparty = Counterparty("Sunrise LLC", "US", isSanctioned = false, rating = 7)
  )

  val sanctionedTrade = Trade(
    id = "T-3",
    notional = BigDecimal("9000000"),
    currency = "GBP",
    isCleared = false,
    counterparty = Counterparty("Opaque Holdings", "RU", isSanctioned = true, rating = 1)
  )
