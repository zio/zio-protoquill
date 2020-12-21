package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.QueryDsl._

object MiniExample3_InlineFilter {
  import io.getquill._
  case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    inline def onlyJoes = 
      (p: Person) => p.name == "Joe"
    
    inline def q = quote {
      query[Person].filter(onlyJoes)
    }

    println( run(q) )

    println( List(Person("Joe", 22), Person("Jack", 33)).filter(onlyJoes) )

  }
}
