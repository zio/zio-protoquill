package io.getquill.context.jasync

import com.github.jasync.sql.db.RowData
import io.getquill.NamingStrategy
import io.getquill.context.Context
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.RunnerSummoningBehavior
import scala.concurrent.ExecutionContext
import io.getquill.context.ContextVerbTranslate
import java.time.ZoneId

trait JAsyncContextBase[D <: SqlIdiom, +N <: NamingStrategy]
  extends Context[D, N]
  with ContextVerbTranslate[D, N]
  with SqlContext[D, N]
  with Decoders
  with Encoders {

  override type PrepareRow = Seq[Any]
  override type ResultRow = RowData

  // Need to define these in ProtoQuill so can pass implicit contexts
  override type RunnerBehavior = RunnerSummoningBehavior.Implicit
  override type Runner = ExecutionContext
  override type Session = Unit

  val dateTimeZone = ZoneId.systemDefault()
}
