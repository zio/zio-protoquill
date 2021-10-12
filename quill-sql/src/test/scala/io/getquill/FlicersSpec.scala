package io.getquill

object FlicersSpec extends Spec {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(firstName: String, lastName: String, age: Int)

  "Filtering Expressed columns" in {
    val columns = List("firstName")
    inline def q = quote {
      query[Person].filterColumns(columns)
    }
    // println(q)
    val r = ctx.run(q)
    // println( r.string )
    // println( r.prepareRow.data.toList)
  }

  // Add using `Person.apply` and using `new Person` to texsts
  // "Case Class Construction" in {
  //   inline def q = quote {
  //     query[Person].map(p => new Person(p.firstName, p.lastName, p.age))
  //   }
  //   println( ctx.run(q) )
  // }
}