package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
// import miniquill.parser.PrintMac
// import miniquill.parser.MatchMac
// import miniquill.parser.MatchLambdaMac

object AdvTest {

  def main(args: Array[String]): Unit = {

    import io.getquill._
    import miniquill.quoter.QueryMeta

    case class Person(id: Int, name: String, age: Int)
    case class PersonName(name: String)

    implicit inline def qm: QueryMeta[PersonName, String] = {
        queryMeta[PersonName, String](
          quote { 
            (q: Query[PersonName]) => q.map(p => p.name)
          }
        )((name: String) => PersonName(name))
      }

    inline def q = quote {
      query[PersonName]
    }

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    
    val output = run(q)
    println(output)
  }
}
