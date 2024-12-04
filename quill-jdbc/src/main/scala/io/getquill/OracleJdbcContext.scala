package io.getquill

import java.io.Closeable
import com.typesafe.config.Config
import io.getquill.context.jdbc.{BooleanIntEncoding, JdbcContext, JdbcContextEncoding, ObjectGenericTimeDecoders, ObjectGenericTimeEncoders, OracleJdbcContextBase, UUIDStringEncoding}
import io.getquill.util.LoadConfig

import java.sql.Types
import javax.sql.DataSource

object OracleJdbcContext {
  trait Codec extends JdbcContextEncoding
    with ObjectGenericTimeEncoders
    with ObjectGenericTimeDecoders
    with BooleanIntEncoding
    with UUIDStringEncoding {

    // Normally it is Types.TIME by in that case Oracle truncates the milliseconds
    protected override def jdbcTypeOfLocalTime = Types.TIMESTAMP
    protected override def jdbcTypeOfOffsetTime = Types.TIME
  }

  object Codec extends Codec
}

class OracleJdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[OracleDialect, N]
  with OracleJdbcContextBase[OracleDialect, N] {
  override val idiom: OracleDialect = OracleDialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  object codec {
    export OracleJdbcContext.Codec.{
      Index => _,
      PrepareRow => _,
      ResultRow => _,
      Session => _,
      Runner => _,
      _
    }
  }
}
