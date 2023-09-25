package io.getquill.generic

import io.getquill.metaprog.etc.StringOrNull
import scala.reflect.ClassTag
import scala.reflect.classTag
import io.getquill.StringCodec
import io.getquill.FromString

// Note: Not using abstract Index parameter in ProtoQuill since it would bleed into most planters
trait GenericEncoder[T, PrepareRow, Session] extends ((Int, T, PrepareRow, Session) => PrepareRow) {
  def apply(i: Int, t: T, row: PrepareRow, session: Session): PrepareRow
}

case class GenericEncoderWithStringFallback[T, PrepareRow, Session](
  nullableEncoder: GenericEncoder[Option[T], PrepareRow, Session],
  stringConverter: Either[String, FromString[T]]
)(classTagExpected: ClassTag[T])
    extends GenericEncoder[Any, PrepareRow, Session] {

  private def classTagFromInstance(t: Any) =
    // if the value is just null, use the original encoder, since value conversion shouldn't mater
    if (t == null) classTagExpected
    else if (t.isInstanceOf[Int]) classTag[Int]
    else if (t.isInstanceOf[Long]) classTag[Long]
    else if (t.isInstanceOf[Short]) classTag[Short]
    else if (t.isInstanceOf[Float]) classTag[Float]
    else if (t.isInstanceOf[Double]) classTag[Double]
    else if (t.isInstanceOf[Boolean]) classTag[Boolean]
    else if (t.isInstanceOf[Byte]) classTag[Byte]
    else ClassTag(t.getClass())

  def apply(i: Int, t: Any, row: PrepareRow, session: Session): PrepareRow =
    val classTagActual = classTagFromInstance(t)
    if (t == null || classTagActual <:< classTagExpected)
      // println(s"=============== ENCODING ${classTagActual}: $t as: ${Option(t.asInstanceOf[T])}")
      nullableEncoder(i, Option(t.asInstanceOf[T]), row, session)
    else
      stringConverter match
        case Right(converter) =>
          // using pprint here because want quotes if it is a string value etc...
          println(
            s"[WARN] The field value: ${pprint(t).plainText} had the type `${classTagActual}` but was expecting the type `${classTagExpected}`. Will attempt to convert to string and use the provided from-string converter: $converter."
          )
          nullableEncoder(i, Option(t.asInstanceOf[T]).map(v => converter.fromString(v.toString)), row, session)
        case Left(_) =>
          throw new IllegalStateException(
            s"The field value: ${pprint(t).plainText} had the type `${classTagActual}` but was expecting the type `${classTagExpected}` and could not summon a from-string converter for the type."
          )
}
