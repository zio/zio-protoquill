package zio.quill

import java.io.Closeable

import javax.sql.DataSource
import com.typesafe.config.Config
import zio.quill.context.jdbc.{ JdbcContext, MysqlJdbcContextBase }
import zio.quill.util.LoadConfig

class MysqlJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[MySQLDialect, N]
  with MysqlJdbcContextBase[MySQLDialect, N] {
  override val idiom: MySQLDialect = MySQLDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))
}
