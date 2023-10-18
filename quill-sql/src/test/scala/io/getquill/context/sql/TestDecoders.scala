package io.getquill.context.sql

import io.getquill.MappedEncoding

case class EncodingTestType(value: String)
case class Number(value: String) extends AnyVal

object Number {
  def withValidation(value: String): Option[Number] =
    if (value.forall(_.isDigit))
      Some(Number(value))
    else
      None
}

trait TestDecoders {
  // In Dotty implicit vals need to be typed so needed to add type annotations here that are not present in Scala2-Quill
  implicit val encodingTestTypeDecoder: MappedEncoding[String, EncodingTestType] =
    MappedEncoding[String, EncodingTestType].apply(EncodingTestType.apply)
  implicit val nameDecoder: MappedEncoding[String, Number] =
    MappedEncoding[String, Number].apply(s =>
      Number
        .withValidation(s)
        .getOrElse(throw new Exception(s"Illegal number $s"))
    )
}
