package verdict.demo

import verdict.*
import verdict.example.{Counterparty, Trade}
import verdict.macros.rule

/** Prints the schema, a rule from each authoring route, the SQL the rule
  * compiles to, and the evidence for three trades.
  *
  * Run it with `sbt "tests/runMain verdict.demo.Demo"`. The output in the
  * README is pasted from a run of this.
  */
object Demo:

  private val schema = Trade.schema

  private val authored: Rule = rule[Trade] { t =>
    t.notional > 1000000 && (Set("DE", "FR", "IT").contains(t.counterparty.jurisdiction) || !t.isCleared)
  }

  private val storedDocument =
    """{"op":"and",
      | "left":  {"op":"gt","path":"notional","value":1000000},
      | "right": {"op":"or",
      |           "left":  {"op":"in","path":"counterparty.jurisdiction","values":["DE","FR","IT"]},
      |           "right": {"op":"not","rule":{"op":"isTrue","path":"isCleared"}}}}""".stripMargin

  private val trades = List(
    Trade("T-1", BigDecimal("2500000"), "EUR", isCleared = true, Counterparty("Banco Uno", "DE", false, 3)),
    Trade("T-2", BigDecimal("125.50"), "USD", isCleared = false, Counterparty("Sunrise LLC", "US", false, 7)),
    Trade("T-3", BigDecimal("9000000"), "GBP", isCleared = false, Counterparty("Opaque Holdings", "RU", true, 1))
  )

  def main(args: Array[String]): Unit =
    section("Schema, derived from the case class")
    println(schema.render)

    section("Rule, written as Scala and read at compile time")
    println(Analysis.describe(authored))

    section("The same rule, loaded from storage")
    RuleJson.load(storedDocument, schema) match
      case Left(errors) => errors.foreach(error => println(s"  rejected: ${error.message}"))
      case Right(loaded) =>
        println(s"  loaded:            ${Analysis.describe(loaded)}")
        println(s"  same as authored:  ${loaded == authored}")

    section("What the rule reads")
    println(Analysis.fieldsUsed(authored).map(_.render).toList.sorted.mkString("  ", ", ", ""))

    section("Compiled to SQL")
    val fragment = SqlInterpreter.toSql(authored, schema, "trades", NamingStrategy.snake)
    println(s"  ${fragment.sql}")
    println(s"  binds: ${fragment.params.map(_.render).mkString(", ")}")

    section("Evidence")
    trades.foreach { trade =>
      Evaluator.evaluate(authored, trade, schema) match
        case Left(error)     => println(s"${trade.id}: ${error.message}")
        case Right(evidence) =>
          println(s"${trade.id} ${if evidence.held then "matches" else "does not match"}")
          println(evidence.render("  "))
          println()
    }

  private def section(title: String): Unit =
    println(s"\n=== $title ===")
