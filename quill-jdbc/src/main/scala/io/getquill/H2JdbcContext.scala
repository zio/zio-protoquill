package io.getquill

import java.io.Closeable
import javax.sql.DataSource
import com.typesafe.config.Config
import io.getquill.context.jdbc.{BooleanObjectEncoding, H2JdbcContextBase, JdbcContext, JdbcContextEncoding, ObjectGenericTimeDecoders, ObjectGenericTimeEncoders, UUIDObjectEncoding}
import io.getquill.util.LoadConfig



object H2JdbcContext {
  trait Codec extends JdbcContextEncoding
    with ObjectGenericTimeEncoders
    with ObjectGenericTimeDecoders
    with BooleanObjectEncoding
    with UUIDObjectEncoding

  object Codec extends Codec
}

class H2JdbcContext[+N <: NamingStrategy](val naming: N, val dataSource: DataSource)
  extends JdbcContext[H2Dialect, N]
  with H2JdbcContextBase[H2Dialect, N] {
  override val idiom: H2Dialect = H2Dialect
  def this(naming: N, config: JdbcContextConfig) = this(naming, config.dataSource)
  def this(naming: N, config: Config) = this(naming, JdbcContextConfig(config))
  def this(naming: N, configPrefix: String) = this(naming, LoadConfig(configPrefix))

  object codec {
    export H2JdbcContext.Codec.{
      Index => _,
      PrepareRow => _,
      ResultRow => _,
      Session => _,
      Runner => _,
      _
    }
  }
}
