package io.getquill

import java.util.Date
import com.datastax.oss.driver.api.core.CqlSession
import io.getquill.context.AstSplicing
import io.getquill.context.cassandra.encoding.{CassandraMapper, CassandraType, MapperSide}
import io.getquill.context.cassandra.{CassandraCodecsBase, CassandraContext, CqlIdiom}
import io.getquill.context.mirror.{MirrorDecoders, MirrorEncoders, MirrorSession, Row}
import io.getquill.context.sql.SqlEncoding

import java.time.{Instant, LocalDate}
import scala.reflect.ClassTag

class CassandraMirrorContextWithQueryProbing extends CassandraMirrorContext(Literal) with QueryProbing

/*
trait CassandraCodecsBase[ResultRow, Session]
  extends ProductDecoders
  with Encoders
  with Decoders
  with CassandraTypes {

  // TODO some kind of global-configuration of timezone? Implicits? System property?
  protected val zoneId = ZoneId.systemDefault

  implicit val timestampDecoder: Decoder[Instant] = decoder[Instant]
  implicit val timestampEncoder: Encoder[Instant] = encoder[Instant]
  implicit val cassandraLocalDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
  implicit val cassandraLocalDateEncoder: Encoder[LocalDate] = encoder[LocalDate]

  //implicit val encodeJava8ZonedDateTime: MappedEncoding[ZonedDateTime, Instant] = MappedEncoding(zdt => zdt.toInstant)
  //implicit val decodeJava8ZonedDateTime: MappedEncoding[Instant, ZonedDateTime] = MappedEncoding(d => ZonedDateTime.ofInstant(d, zoneId))

  val idiom = CqlIdiom
  type BaseNullChecker = GenericNullChecker[ResultRow, Session]
  type NullChecker = CassandraNullChecker
  class CassandraNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.isNull(index)
  }
  implicit val nullChecker: NullChecker = new CassandraNullChecker()
}
 */

object CassandraMirrorContext extends ProductDecoders with SqlEncoding with MirrorDecoders with MirrorEncoders {
  override type Session = MirrorSession
  override type PrepareRow = Row
  override type ResultRow = Row
  override type NullChecker = MirrorNullChecker

  implicit val timestampDecoder: Decoder[Instant] = decoder[Instant]
  implicit val timestampEncoder: Encoder[Instant] = encoder[Instant]
  implicit val cassandraLocalDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
  implicit val cassandraLocalDateEncoder: Encoder[LocalDate] = encoder[LocalDate]

  class MirrorNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.nullAt(index)
  }

  implicit val nullChecker: NullChecker = new MirrorNullChecker()

  implicit def listDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[List[T]] = decoderUnsafe[List[T]]
  implicit def setDecoder[T, Cas](implicit mapper: CassandraMapper[Cas, T, MapperSide.Decode], ct: ClassTag[Cas]): Decoder[Set[T]] = decoderUnsafe[Set[T]]
  implicit def mapDecoder[K, V, KCas, VCas](
    implicit
    keyMapper: CassandraMapper[KCas, K, MapperSide.Decode],
    valMapper: CassandraMapper[VCas, V, MapperSide.Decode],
    a: ClassTag[KCas],
    b: ClassTag[VCas]
  ): Decoder[Map[K, V]] = decoderUnsafe[Map[K, V]]

  implicit def listEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode], ct: ClassTag[Cas]): Encoder[List[T]] = encoder[List[T]]
  implicit def setEncoder[T, Cas](implicit mapper: CassandraMapper[T, Cas, MapperSide.Encode], ct: ClassTag[Cas]): Encoder[Set[T]] = encoder[Set[T]]
  implicit def mapEncoder[K, V, KCas, VCas](
    implicit
    keyMapper: CassandraMapper[K, KCas, MapperSide.Encode],
    valMapper: CassandraMapper[V, VCas, MapperSide.Encode],
    a: ClassTag[KCas],
    b: ClassTag[VCas]
  ): Encoder[Map[K, V]] = encoder[Map[K, V]]

  implicit def udtCassandraType[T <: Udt]: CassandraType[T] = CassandraType.of[T]
  implicit def udtDecoder[T <: Udt: ClassTag]: Decoder[T] = decoder[T]
  implicit def udtEncoder[T <: Udt]: Encoder[T] = encoder[T]
}

class CassandraMirrorContext[+Naming <: NamingStrategy](val naming: Naming)
  extends MirrorContextBase[CqlIdiom, Naming]
  with CassandraContext[Naming]
  with AstSplicing {

  val idiom = CqlIdiom
  val session: MirrorSession = MirrorSession("DefaultMirrorContextSession")

  export CassandraMirrorContext.{
    PrepareRow => _,
    ResultRow => _,
    Session => _,
    _
  }
}