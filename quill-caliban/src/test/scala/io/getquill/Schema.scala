package io.getquill

import io.getquill.jdbczio.Quill.Postgres.Codec.*

object FlatSchema {
  case class PersonT(id: Int, first: String, last: String, age: Int)
  case class AddressT(ownerId: Int, street: String)
  case class PersonAddress(id: Int, first: String, last: String, age: Int, street: Option[String])
  given CompositeDecoder[PersonT] = deriveComposite
  given CompositeDecoder[AddressT] = deriveComposite
  given CompositeDecoder[PersonAddress] = deriveComposite

  object ExampleData {
    val people =
      List(
        PersonT(1, "One", "A", 44),
        PersonT(2, "Two", "B", 55),
        PersonT(3, "Three", "C", 66)
      )
    val addresses =
      List(
        AddressT(1, "123 St"),
        AddressT(2, "789 St")
      )
    val personAddress =
      List(
        PersonAddress(1, "One", "A", 44, Some("123 St")),
        PersonAddress(2, "Two", "B", 55, Some("123 St")),
        PersonAddress(3, "Three", "C", 66, None),
      )
  }
}

object NestedSchema {
  case class Name(first: String, last: String)
  case class PersonT(id: Int, name: Name, age: Int)
  case class AddressT(ownerId: Int, street: String)
  // Needs to be named differently from Flat.PersonAddress___ since Caliban infers from this class & name must be different
  case class PersonAddressNested(id: Int, name: Name, age: Int, street: Option[String])
  given CompositeDecoder[Name] = deriveComposite
  given CompositeDecoder[PersonT] = deriveComposite
  given CompositeDecoder[AddressT] = deriveComposite
  given CompositeDecoder[PersonAddressNested] = deriveComposite


  object ExampleData {
    val people =
      List(
        PersonT(1, Name("One", "A"), 44),
        PersonT(2, Name("Two", "B"), 55),
        PersonT(3, Name("Three", "C"), 66)
      )
    val addresses =
      List(
        AddressT(1, "123 St"),
        AddressT(2, "789 St")
      )
    val personAddress =
      List(
        PersonAddressNested(1, Name("One", "A"), 44, Some("123 St")),
        PersonAddressNested(2, Name("Two", "B"), 55, Some("123 St")),
        PersonAddressNested(3, Name("Three", "C"), 66, None),
      )  }
}
