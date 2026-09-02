package verdict.example

/** The record type used by the tests, the differential test and the demo. */
final case class Counterparty(
    name: String,
    jurisdiction: String,
    isSanctioned: Boolean,
    rating: Int
)

final case class Trade(
    id: String,
    notional: BigDecimal,
    currency: String,
    isCleared: Boolean,
    counterparty: Counterparty
)
