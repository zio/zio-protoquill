package io.getquill.context.jasync

import io.getquill.ReturnAction.{ReturnColumns, ReturnNothing, ReturnRecord}
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.{Context, ExecutionInfo}
import io.getquill.util.ContextLogger
import io.getquill.{NamingStrategy, ReturnAction}

import java.sql.*
import java.util.TimeZone

trait JAsyncContextTypes[Dialect <: SqlIdiom, Naming <: NamingStrategy] extends Context[Dialect, Naming]
  with SqlContext[Dialect, Naming]
  with Encoders
  with Decoders {

  // Dotty doesn't like that this is defined in both Encoders and Decoders.
  // Makes us define it here in order to resolve the conflict.
  type Index = Int
  type PrepareRow = PreparedStatement
  type ResultRow = ResultSet
  type Session = Connection
  type Runner = Unit

  override type NullChecker = JdbcNullChecker
  class JdbcNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: ResultSet): Boolean = {
      // Note that JDBC-rows are 1-indexed
      row.getObject(index + 1) == null
    }
  }
  implicit val nullChecker: JdbcNullChecker = new JdbcNullChecker()

  protected val dateTimeZone = TimeZone.getDefault
  
  
}