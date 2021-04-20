package io.getquill

case class Name(value: String) extends AnyVal // hello

class AnyValEncodingSpec extends Spec {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  case class Person(name: Name, age:Int)

  "simple anyval should encode and decode" in {
    val mirror = ctx.run(query[Person].filter(p => p.name == lift(Name("Joe"))))
    println(mirror)
  }
}