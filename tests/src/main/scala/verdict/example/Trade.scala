package verdict.example

import verdict.{Identity, Schema, Sensitive}

/** The record type used by the tests, the differential test and the demo. */
final case class Counterparty(
    @Sensitive name: String,
    jurisdiction: String,
    isSanctioned: Boolean,
    rating: Int
) derives Schema

final case class Trade(
    @Identity id: String,
    notional: BigDecimal,
    currency: String,
    isCleared: Boolean,
    counterparty: Counterparty
) derives Schema

object Trade:
  /** The tagged schema, read through the macro. `derives Schema` gives the same
    * shape with empty annotation sets.
    */
  val schema: Schema[Trade] = verdict.macros.Annotated.schema[Trade]
