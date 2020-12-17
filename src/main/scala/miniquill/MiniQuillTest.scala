package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.QueryDsl._

object AdvTest {

  def main(args: Array[String]): Unit = {

    import io.getquill._
    import miniquill.quoter.QueryMeta

    case class Person(id: Int, name: String, age: Int)
    case class Address(street: String, zip: Int, personId: Int)

    // case class PersonName(name: String)

    // implicit inline def qm: QueryMeta[PersonName, String] = {
    //     queryMeta[PersonName, String](
    //       quote { 
    //         (q: Query[PersonName]) => q.map(p => p.name)
    //       }
    //     )((name: String) => PersonName(name))
    //   }

    // val q = quote {
    //   query[PersonName]
    // }

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    // inline def q = quote {
    //   query[Person].filter(p => p.name == lift("joe"))
    // }
    // inline def result = run(q)
    // println( result.string(true) )
    // println( result.prepareRow.data.toList )

    // inline def q1 = quote {
    //   query[Person].join(query[Address]).on((p, a) => p.id == a.personId)
    // }
    // inline def result1 = run(q1)
    // println( result1.string(true) )
    // println( result1.prepareRow.data.toList )

    inline def q = quote {
      query[Person].insert(_.name -> "joe")
    }
    //println( run(q) )

  }
}
