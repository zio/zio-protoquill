package io.getquill

import io.getquill.context.ExecutionType

class MirrorEncodingSpec extends Spec {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  "simple encoder/decoder - class" in {
    class Name(val value: String)
    case class Person(name: Name, age: Int)
    implicit val encodeName: MappedEncoding[Name, String] = MappedEncoding[Name, String](_.value)
    implicit val decodeName: MappedEncoding[String, Name] = MappedEncoding[String, Name](str => Name(str))

    val name = Name("Joe")
    val mirror = ctx.run(query[Person].filter(p => p.name == lift(name)))
    mirror.triple mustEqual (
      (
        "SELECT p.name, p.age FROM Person p WHERE p.name = ?",
        List("Joe"),
        ExecutionType.Static
      )
    )
  }

  "simple encoder/decoder - case class" in {
    case class Name(value: String)
    case class Person(name: Name, age: Int)
    implicit val encodeName: MappedEncoding[Name, String] = MappedEncoding[Name, String](_.value)
    implicit val decodeName: MappedEncoding[String, Name] = MappedEncoding[String, Name](str => Name(str))

    val name = Name("Joe")
    val mirror = ctx.run(query[Person].filter(p => p.name == lift(name)))
    mirror.triple mustEqual (
      (
        "SELECT p.name, p.age FROM Person p WHERE p.name = ?",
        List("Joe"),
        ExecutionType.Static
      )
    )
  }

  // This should technically work but does not compile because in the ExpandNestedQueries.scala phase it causes a:
  // io.getquill.quotation.QuatException: The post-rename field 'value' does not exist in an SQL-level type V
  // Should look into that transformation phase and see if it can be changed to be tolerant of nonexistent fields in quats
  // "simple encoder/decoder - case clas - do not need decoder" in {
  //   case class Name(value: String)
  //   case class Person(name: Name, age:Int)
  //   implicit val encodeName: MappedEncoding[Name, String] = MappedEncoding[Name, String](_.value)
  //   //implicit val decodeName: MappedEncoding[String, Name] = MappedEncoding[String, Name](str => Name(str))

  //   val name = Name("Joe")
  //   val mirror = ctx.run(query[Person].filter(p => p.name == lift(name)))
  //   mirror.triple mustEqual ((
  //     "SELECT p.name, p.age FROM Person p WHERE p.name = ?",
  //     List("Joe"),
  //     ExecutionType.Static
  //   ))
  // }

  // If we don't specify a mapped-encoding for `Name` it is treated as am embedded class.
  // i.e. it's parameter `value` is used for the decoding instead of the `name` parameter of Person
  // for the select (since the mirror SQL dialect collapses the properties). However, it is still
  // p.name for the selector.
  "simple encoder/decoder - case class - do not need encoder or decoder" in {
    case class Name(value: String)
    case class Person(name: Name, age: Int)

    val name = Name("Joe")
    val mirror = ctx.run(query[Person].filter(p => p.name == lift(name)))
    mirror.triple mustEqual (
      (
        "SELECT p.value, p.age FROM Person p WHERE p.name = ?",
        List("Joe"),
        ExecutionType.Static
      )
    )

    // When you refer to the value of the parameter "all the way inside" the p.name.value then it will collapse
    // the property p.name.value once the query is synthesized.
    val mirror2 = ctx.run(query[Person].filter(p => p.name.value == lift("Joe")))
    mirror2.triple mustEqual (
      (
        "SELECT p.value, p.age FROM Person p WHERE p.value = ?",
        List("Joe"),
        ExecutionType.Static
      )
    )
  }
}
