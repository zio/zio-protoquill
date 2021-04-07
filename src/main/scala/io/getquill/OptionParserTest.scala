// package io.getquill

// import io.getquill.quoter.Dsl._
// import io.getquill.quoter.Dsl.autoQuote
// import io.getquill.parser._

// object OptionParserTest {

//   def main(args: Array[String]): Unit = {
//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     import ctx._

//     // val x : String = "h"
//     // val y : String = "o"

//     // val output = quote{ x.toLowerCase }//compile
//     // println(output)
//     case class Person(id: Int, name: Option[String])
//     case class Address(street: String, zip: Int, fk: Option[Int])
//     case class Company(name: String, zip: Int)

//     val nonEmpty = quote {
//       query[Person].filter(p => p.name.isDefined)
//     }
//     println(nonEmpty)
//     println(ctx.run(nonEmpty))

//     // ===============================================

//     // val exists = quote {
//     //   query[Person].join(query[Address]).on((p, a)=> a.fk.exists(_ == p.id))
//     // }
//     // println(exists)
//     // println(ctx.run(exists))

// // ===============================================
    

//     // val mp = quote {
//     //   query[Company]
//     //     .leftJoin(query[Address])
//     //     .on((c, a) => c.zip == a.zip)
//     //     .map((c,a) => (c.name, a.map(_.street)))
//     // }
//     // println(mp)
//     // println(ctx.run(mp))

    
//     // val contains = quote {
//     //   query[Person]
//     //     .leftJoin(query[Address])
//     //     .on((p, a) => a.fk.contains(12345))
//     //     .map((p,a) => (p.name, a))
//     // }
//     // println(contains)
//     // println(ctx.run(contains))
//   }
// }
