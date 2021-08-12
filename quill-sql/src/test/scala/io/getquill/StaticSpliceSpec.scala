// package io.getquill

// import io.getquill.util.prep.Constants

// class StaticSpliceSpec extends Spec {
//   val ctx = new SqlMirrorContext(PostgresDialect, Literal)
//   import ctx._

//   case class Person(name: String, age: Int)

//   "static splice should summon" - {
//     "and transpile to a query" in {
//       ctx.run { query[Person].filter(p => p.name == static(Constants.Joe)) } //hello
//     }
//   }
// }