package io.getquill.examples

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.quoter.QueryDsl._

object MiniExample_LiftByKeys {
  import io.getquill._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  import io.getquill.metaprog.MapFilterator

  case class Person(firstName: String, lastName: String, age: Int)

  val values: Map[String, String] = Map("firstName" -> "Joe", "age" -> "22")

  def regularMapProc() = {
    inline def q = quote {
      query[Person].filter(p => 
        MapFilterator[Person, PrepareRow](p, values, null, (a, b) => (a == b) || (b == (null) ) )
      )
    }
    val r = run(q)
    println( r.string )
    println( r.prepareRow.data.toList) 
  }

  extension [T](inline q: EntityQuery[T]) {
    inline def filterByKeys(inline map: Map[String, String]) =
      q.filter(p => MapFilterator[T, PrepareRow](p, map, null, (a, b) => (a == b) || (b == (null) ) ))
  }

  def extensionMapProc() = {
    inline def q = quote {
      query[Person].filterByKeys(values)
    }
    val r = run(q)
    println( r.string )
    println( r.prepareRow.data.toList) 
  }

  /* 
   ============= The following expasion happens ===========
   SELECT p.firstName, p.lastName, p.age FROM Person p 
   WHERE 
     ( p.firstName = [ values.getOrElse("firstName",null) ] OR [ values.getOrElse("firstName",null) ] == null ) AND 
     ( p.lastName = [ values.getOrElse("lastName",null) ] OR [ values.getOrElse("lastName",null) ] == null ) AND
     ( p.age = [ values.getOrElse("age",null) ] OR [ values.getOrElse("age",null) == null ] ) AND true
  */

  def main(args: Array[String]): Unit = {
    regularMapProc()
    extensionMapProc()
  }
  
}
