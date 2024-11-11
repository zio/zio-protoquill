package io.getquill

import java.io.Closeable
import javax.sql.DataSource
import com.typesafe.config.Config
import io.getquill.context.jdbc.{ArrayDecoders, ArrayEncoders, BooleanObjectEncoding, JdbcContext, JdbcContextEncoding, ObjectGenericTimeDecoders, ObjectGenericTimeEncoders, PostgresJdbcContextBase, UUIDObjectEncoding}
import io.getquill.util.LoadConfig

import java.sql.Types

// TODO turn this into a trait because need to have a similar thing for the ZIO context
//     (same with all of the other context objects)
trait PostgresJdbcContextModule extends JdbcContextEncoding
  with ObjectGenericTimeEncoders
  with ObjectGenericTimeDecoders
  with BooleanObjectEncoding
  with UUIDObjectEncoding
  with ArrayDecoders
  with ArrayEncoders {
  // Postgres does not support Types.TIME_WITH_TIMEZONE as a JDBC type but does have a `TIME WITH TIMEZONE` datatype this is puzzling.
  protected override def jdbcTypeOfOffsetTime = Types.TIME

  override def parseJdbcType(intType: Int): String = intType match {
    case Types.TINYINT => parseJdbcType(Types.SMALLINT)
    case Types.VARCHAR => "text"
    case Types.DOUBLE => "float8"
    case _ => parseJdbcType(intType)
  }
}

object PostgresJdbcContext extends PostgresJdbcContextModule

class PostgresJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[PostgresDialect, N]
  with PostgresJdbcContextBase[PostgresDialect, N] {
  override val idiom: PostgresDialect = PostgresDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  export PostgresJdbcContext.{
    Index => _,
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    Runner => _,
    _
  }
}
