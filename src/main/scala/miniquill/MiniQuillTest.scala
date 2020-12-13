// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.Dsl._
// import miniquill.quoter.QueryDsl._

// object AdvTest {

//   def main(args: Array[String]): Unit = {

//     import io.getquill._
//     import miniquill.quoter.QueryMeta

//     case class Person(id: Int, name: String, age: Int)
//     // case class PersonName(name: String)

//     // implicit inline def qm: QueryMeta[PersonName, String] = {
//     //     queryMeta[PersonName, String](
//     //       quote { 
//     //         (q: Query[PersonName]) => q.map(p => p.name)
//     //       }
//     //     )((name: String) => PersonName(name))
//     //   }

//     // val q = quote {
//     //   query[PersonName]
//     // }

//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     import ctx._

//     val q = quote {
//       query[Person].filter(p => p.name == lift("joe"))
//     }


    
//     val result = run(q)
//     println( result.string(true) )
//     println( result.prepareRow.data.toList )


//   }
// }
