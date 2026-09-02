package verdict

import zio.test.*

object FieldPathSpec extends ZIOSpecDefault:

  def spec = suite("FieldPath")(
    test("accepts a simple name") {
      assertTrue(FieldPath.parse("notional").map(_.render) == Right("notional"))
    },
    test("accepts a nested path and splits it into segments") {
      val parsed = FieldPath.parse("counterparty.jurisdiction")
      assertTrue(
        parsed.map(_.segments) == Right(List("counterparty", "jurisdiction")),
        parsed.map(_.isNested) == Right(true)
      )
    },
    test("rejects an empty path") {
      assertTrue(FieldPath.parse("").isLeft)
    },
    test("rejects an empty segment and says why") {
      val error = FieldPath.parse("counterparty..jurisdiction").swap.toOption.get
      assertTrue(error.contains("empty segment"))
    },
    test("rejects a segment that is not a legal field name") {
      assertTrue(
        FieldPath.parse("counterparty.9lives").isLeft,
        FieldPath.parse("counter-party").isLeft,
        FieldPath.parse("notional ").isLeft
      )
    },
    test("unsafe throws on a path parse would reject") {
      val thrown = scala.util.Try(FieldPath.unsafe("not a path"))
      assertTrue(thrown.isFailure)
    }
  )
