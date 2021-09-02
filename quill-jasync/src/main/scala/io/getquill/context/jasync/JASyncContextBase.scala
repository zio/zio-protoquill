package io.getquill.context.jasync

import com.github.jasync.sql.db.RowData
import io.getquill.NamingStrategy
import io.getquill.context.Context
import io.getquill.context.sql.SqlContext
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.DatasourceContextInjection
import scala.concurrent.ExecutionContext

trait JAsyncContextBase[D <: SqlIdiom, N <: NamingStrategy]
  extends Context[D, N]
  //with TranslateContext
  with SqlContext[D, N]
  with Decoders
  with Encoders {

  override type PrepareRow = Seq[Any]
  override type ResultRow = RowData

  // Need to define these in ProtoQuill so can pass implicit contexts
  override type DatasourceContextBehavior = DatasourceContextInjection.Implicit
  override type DatasourceContext = ExecutionContext
  override type Session = Unit
}
