package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._

object MiniExample_OneOf {

  
  import io.getquill.metaprog.etc._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  inline def oneOf(inline list: List[String], inline column:String): Boolean = {
    inline if (ListFlicer.isNil(list))
      false
    else
      ListFlicer.index(list, 0) == column || oneOf(ListFlicer.tail(list), column)
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
