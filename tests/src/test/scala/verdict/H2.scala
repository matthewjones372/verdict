package verdict

import verdict.example.Trade

import java.sql.{Connection, DriverManager, PreparedStatement}
import zio.*

/** The database side of the differential test.
  *
  * The connection is a resource, so it is acquired and released rather than
  * opened and hoped about; the schema is created once for the suite that uses
  * the layer.
  */
object H2:

  private val createTable =
    """CREATE TABLE trades (
      |  id                         VARCHAR(32) PRIMARY KEY,
      |  notional                   NUMERIC(20, 4) NOT NULL,
      |  currency                   VARCHAR(16) NOT NULL,
      |  is_cleared                 BOOLEAN NOT NULL,
      |  counterparty_name          VARCHAR(64) NOT NULL,
      |  counterparty_jurisdiction  VARCHAR(16) NOT NULL,
      |  counterparty_is_sanctioned BOOLEAN NOT NULL,
      |  counterparty_rating        INTEGER NOT NULL
      |)""".stripMargin

  val layer: ZLayer[Any, Throwable, Connection] =
    ZLayer.scoped {
      ZIO.acquireRelease(
        ZIO.attemptBlocking {
          val connection = DriverManager.getConnection("jdbc:h2:mem:verdict;DB_CLOSE_DELAY=-1")
          val statement  = connection.createStatement()
          try statement.execute(createTable)
          finally statement.close()
          connection
        }
      )(connection => ZIO.succeed(connection.close()))
    }

  def replaceAll(connection: Connection, trades: List[Trade]): Task[Unit] =
    ZIO.attemptBlocking {
      val truncate = connection.createStatement()
      try truncate.execute("DELETE FROM trades")
      finally truncate.close()

      val insert = connection.prepareStatement(
        "INSERT INTO trades VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      )
      try
        trades.foreach { trade =>
          insert.setString(1, trade.id)
          insert.setBigDecimal(2, trade.notional.bigDecimal)
          insert.setString(3, trade.currency)
          insert.setBoolean(4, trade.isCleared)
          insert.setString(5, trade.counterparty.name)
          insert.setString(6, trade.counterparty.jurisdiction)
          insert.setBoolean(7, trade.counterparty.isSanctioned)
          insert.setInt(8, trade.counterparty.rating)
          insert.addBatch()
        }
        insert.executeBatch()
        ()
      finally insert.close()
    }

  /** Runs a compiled rule and returns the ids it selects. */
  def select(connection: Connection, fragment: SqlFragment): Task[Set[String]] =
    ZIO.attemptBlocking {
      val statement = connection.prepareStatement(fragment.sql)
      try
        bind(statement, fragment.params)
        val rows    = statement.executeQuery()
        val builder = Set.newBuilder[String]
        try while rows.next() do builder += rows.getString("id")
        finally rows.close()
        builder.result()
      finally statement.close()
    }

  private def bind(statement: PreparedStatement, params: List[FieldValue]): Unit =
    params.zipWithIndex.foreach { (value, index) =>
      val position = index + 1
      value match
        case FieldValue.Text(v) => statement.setString(position, v)
        case FieldValue.Num(v)  => statement.setBigDecimal(position, v.bigDecimal)
        case FieldValue.Flag(v) => statement.setBoolean(position, v)
    }
