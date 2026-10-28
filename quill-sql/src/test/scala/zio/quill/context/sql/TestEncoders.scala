package zio.quill.context.sql

import zio.quill.MappedEncoding

trait TestEncoders {
  // In Dotty implicit vals need to be typed so needed to add type annotations here that are not present in Scala2-Quill
  implicit val encodingTestTypeEncoder: MappedEncoding[EncodingTestType, String] = MappedEncoding[EncodingTestType, String](_.value)
  implicit val nameEncoder: MappedEncoding[Number, String] = MappedEncoding[Number, String](_.value)
}
