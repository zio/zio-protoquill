package io.getquill.examples

import io.getquill.MirrorContext.Codec.*

object MiniExampleEntities {
  case class Person(name: String, age: Int)
  given CompositeDecoder[Person] = deriveComposite
}
