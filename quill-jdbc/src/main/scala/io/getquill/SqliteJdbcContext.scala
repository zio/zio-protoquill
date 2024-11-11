package io.getquill

import java.io.Closeable
import javax.sql.DataSource
import com.typesafe.config.Config
import io.getquill.context.jdbc.{BasicTimeDecoders, BasicTimeEncoders, BooleanObjectEncoding, JdbcContext, JdbcContextEncoding, SqliteJdbcContextBase, UUIDObjectEncoding}
import io.getquill.util.LoadConfig

trait SqliteJdbcContextModule extends JdbcContextEncoding
  with BasicTimeEncoders
  with BasicTimeDecoders
  with BooleanObjectEncoding
  with UUIDObjectEncoding

object SqliteJdbcContext extends SqliteJdbcContextModule

class SqliteJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[SqliteDialect, N]
  with SqliteJdbcContextBase[SqliteDialect, N] {
  override val idiom: SqliteDialect = SqliteDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  export SqliteJdbcContext.{
    Index => _,
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    Runner => _,
    _
  }
}
