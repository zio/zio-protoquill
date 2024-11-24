package io.getquill

import io.getquill.generic.{DecodingType, GenericDecoder}

// TODO ProtoQuill needs it's own MappedEncoding class. That should
//      be moved out of the Quill base Jar. Maybe do it in the next quill release
//      or maybe just implement the below as an extension method
extension [I, O](mappedEncoding: MappedEncoding[I, O]) {
  def toDecoder[ResultRow, Session, DT <: DecodingType](implicit decoder: GenericDecoder[ResultRow, Session, I, DT]) =
    decoder.map { (elem: I) => mappedEncoding.f(elem) }
}
