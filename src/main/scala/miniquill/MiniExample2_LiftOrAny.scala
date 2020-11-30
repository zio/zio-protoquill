package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.QueryDsl._

object MiniExample2_LiftOrAny {
  import io.getquill._
  case class Person(name: String, age: Int)


  def main(args: Array[String]): Unit = { //hello

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    
    inline def liftOrAny(inline field: String, inline filter: Option[String]) =
      field.like(lift(filter.getOrElse("%")))

    val runtimeValue = Some("Joe")
    inline def q = quote {
      query[Person].filter(p => liftOrAny(p.name, runtimeValue))
    }

    println( run(q) )

  }
}
