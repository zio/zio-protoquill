package io.getquill.generic

import io.getquill.metaprog.etc.StringOrNull
import scala.reflect.ClassTag
import scala.reflect.classTag

// Note: Not using abstract Index parameter in ProtoQuill since it would bleed into most planters
trait GenericEncoder[T, PrepareRow, Session] extends ((Int, T, PrepareRow, Session) => PrepareRow) {
  def apply(i: Int, t: T, row: PrepareRow, session: Session): PrepareRow
}

case class GenericEncoderWithStringFallback[T, PrepareRow, Session](
    original: GenericEncoder[T, PrepareRow, Session],
    fallback: GenericEncoder[String, PrepareRow, Session],
    badExpressionLog: String = ""
)(classTagExpected: ClassTag[T]) extends GenericEncoder[Any, PrepareRow, Session] {
  def apply(i: Int, t: Any, row: PrepareRow, session: Session): PrepareRow =
    val classTagActual =
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

    if (t == null)
      original(i, t.asInstanceOf[T], row, session)
    else if (classTagActual <:< classTagExpected)
      original(i, t.asInstanceOf[T], row, session)
    else
      // using pprint here because want quotes if it is a string value etc...
      println(s"[WARN] The field value: ${pprint(t).plainText} had the type `${classTagActual}` but was expecting the type `${classTagExpected}`.${badExpressionLog}")
      fallback(i, StringOrNull(t), row, session)
}
