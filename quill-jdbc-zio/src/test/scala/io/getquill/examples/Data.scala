package io.getquill.examples

import io.getquill.jdbczio.Quill.Postgres.Codec.*

object Data {
  case class Person(name: String, age: Int)
  given CompositeDecoder[Person] = deriveComposite
}
