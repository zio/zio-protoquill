package io.getquill

import java.io.Closeable

import javax.sql.DataSource
import com.typesafe.config.Config
import io.getquill.context.jdbc.{JdbcContext, SqlServerJdbcContextBase}
import io.getquill.util.LoadConfig

class SqlServerJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
    extends JdbcContext[SQLServerDialect, N]
    with SqlServerJdbcContextBase[SQLServerDialect, N] {
  override val idiom: SQLServerDialect = SQLServerDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))
}
