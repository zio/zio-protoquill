package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._

object MiniExample_OneOf {

  import io.getquill._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  inline def oneOf(inline list: List[String], inline column:String): Boolean = {
    inline if (ListProc.isNil(list))
      false
    else
      ListProc.index(list, 0) == column || oneOf(ListProc.tail(list), column)
  }

  case class Person(name: String, age: Int)

  inline def q = quote {
    query[Person].filter(p => oneOf(List("Joe", "Jack"), p.name))
  }
  println( run(q) )

  case class Node(status: String, lastStatus: String, backupStatus: String)

  inline def q1 = quote {
    query[Node].filter(n => oneOf(List(n.lastStatus, n.backupStatus), n.status))
  }
  println( run(q1) )

  inline def q2 = quote {
    query[Node].filter(n => oneOf(List(n.lastStatus, "restarting"), n.status))
  }
  println( run(q2) )

  def main(args: Array[String]): Unit = {
    
  }
}
