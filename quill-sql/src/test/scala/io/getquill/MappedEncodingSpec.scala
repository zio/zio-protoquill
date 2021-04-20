// package io.getquill

// object MappedEncodingSpec extends Spec {

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   case class Name(value: String) extends AnyVal
//   case class Person(name: Name, age:Int)

//   "simple anyval should encode and decode" in {
//     ctx.run(query[Person])
//   }
// }