package io.getquill

import java.io.Closeable
import javax.sql.DataSource
import com.typesafe.config.Config
import io.getquill.context.jdbc.{BooleanObjectEncoding, JdbcContext, JdbcContextEncoding, MysqlJdbcContextBase, ObjectGenericTimeDecoders, ObjectGenericTimeEncoders, UUIDStringEncoding}
import io.getquill.util.LoadConfig

import java.sql.Types

trait MysqlJdbcContextModule extends JdbcContextEncoding
  with ObjectGenericTimeEncoders
  with ObjectGenericTimeDecoders
  with BooleanObjectEncoding
  with UUIDStringEncoding {

  protected override def jdbcTypeOfZonedDateTime = Types.TIMESTAMP
  protected override def jdbcTypeOfInstant = Types.TIMESTAMP
  protected override def jdbcTypeOfOffsetTime = Types.TIME
  protected override def jdbcTypeOfOffsetDateTime = Types.TIMESTAMP
}

object MysqlJdbcContext extends MysqlJdbcContextModule

class MysqlJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[MySQLDialect, N]
  with MysqlJdbcContextBase[MySQLDialect, N] {
  override val idiom: MySQLDialect = MySQLDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  export MysqlJdbcContext.{
    Index => _,
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    Runner => _,
    _
  }
}
