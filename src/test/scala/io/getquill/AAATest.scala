package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill.quoter.QueryDsl._
import io.getquill.quoter.SchemaMeta
import io.getquill.quoter.QueryMeta
import io.getquill.quoter.InsertMeta
import io.getquill.parser.PrintMac

object AAATest {
  import io.getquill._

  case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {
    // TODO Test to make sure that at runtime, lazy lifts not supported
    inline def q = quote {
      query[Person].filter(p => p.name == lazyLift("Joe"))
    }
    PrintMac(q)

    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    val r = run(q)
    println( r.string )
    println( r.prepareRow.data.toList)
  }
}