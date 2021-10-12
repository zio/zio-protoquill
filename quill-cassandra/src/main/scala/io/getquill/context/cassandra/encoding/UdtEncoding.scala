package io.getquill.context.cassandra.encoding

import com.datastax.driver.core.UDTValue
import io.getquill.Udt
import io.getquill.context.cassandra.CassandraRowContext
import io.getquill.generic.EncodingDsl
import io.getquill.context.{ CassandraSession, ExecutionInfo, Context, UdtValueLookup }
import com.datastax.driver.core.Row
import com.datastax.driver.core.BoundStatement
import scala.util.NotGiven

/**
 * UdtDecoding and UdtEncoding classes govern implicit summoning of Udt Encoders/Decoders.
 * Unlike Scala2-Quill, in ProtoQuill these need to be separate classes since
 * if would have encoders & decoders in one, Encoders self-type would also need to to contain decoders.
 * Also note that because I have decided to allow the types Decoder[_], Encoder[_] to be parameters,
 * despite the fact that really it is CassandraEncoder and CassandraDecoder that are used everywhere.
 * This is to allow future Cassandra context implementations not to have to use that.
 * Also, note that just like EncodingDsl.anyValEncoder/Decoder, this implementation is subject
 * to the problem of https://github.com/lampepfl/dotty/issues/12179 and therefore needs to have the same
 * workaround i.e. having the macro call returning a `CassandraDecoderMaker[Decoder, T] => Decoder[T]`
 * as opposed to being able to use `self.decoder/encoder` in the macro directly.
 */
trait UdtDecoding extends CassandraRowContext { self: Decoders =>
  implicit inline def udtDecoder[T <: Udt](using NotGiven[T =:= Udt]): Decoder[T] =
    UdtDecodingMacro.udtDecoder(
      new CassandraDecoderMaker[Decoder, T] {
        def apply(e: (Int, Row, UdtValueLookup) => T): Decoder[T] = self.decoder(e)
      }
    )

  implicit inline def udtDecodeMapper[T <: Udt](using NotGiven[T =:= Udt]): CassandraMapper[UDTValue, T, MapperSide.Decode] =
    UdtDecodingMacro.udtDecodeMapper(
      new CassandraDecodeMapperMaker[Decoder, T] {
        def apply(f: (UDTValue, UdtValueLookup) => T): CassandraMapper[UDTValue, T, MapperSide.Decode] = CassandraMapper(f)
      }
    )
}

trait UdtEncoding extends CassandraRowContext { self: Encoders =>
  implicit inline def udtEncoder[T <: Udt]: Encoder[T] =
    UdtEncodingMacro.udtEncoder(
      new CassandraEncoderMaker[Encoder, T] {
        def apply(e: (Int, T, BoundStatement, UdtValueLookup) => BoundStatement): Encoder[T] = self.encoder(e)
      }
    )

  // implicit def udtDecodeMapper[T <: Udt]: CassandraMapper[UDTValue, T] = macro UdtEncodingMacro.udtDecodeMapper[T]
  implicit inline def udtEncodeMapper[T <: Udt]: CassandraMapper[T, UDTValue, MapperSide.Encode] =
    UdtEncodingMacro.udtEncoderMapper(
      new CassandraEncodeMapperMaker[Encoder, T] {
        def apply(f: (T, UdtValueLookup) => UDTValue): CassandraMapper[T, UDTValue, MapperSide.Encode] = CassandraMapper(f)
      }
    )

}
