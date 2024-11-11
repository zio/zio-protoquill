package io.getquill.context.jdbc

import java.sql.Types

trait BooleanObjectEncoding {
  this: JdbcContextTypes with Encoders with Decoders =>

  implicit val booleanEncoder: Encoder[Boolean] = encoder(Types.BOOLEAN, _.setBoolean)
  implicit val booleanDecoder: Decoder[Boolean] = decoder(_.getBoolean)
}
