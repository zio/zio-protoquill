package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._
import scala.compiletime.{erasedValue, summonFrom, constValue}

import java.sql.Connection

object MiniExample_LiftByKeys {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  import io.getquill.metaprog.etc.MapFlicer

  case class Person(firstName: String, lastName: String, age: Int)

  val values: Map[String, String] = Map("firstName" -> "Joe", "age" -> "22")

  def regularMapProc() = {
    inline def q = quote {
      query[Person].filter(p =>
        MapFlicer[Person, PrepareRow, Session](p, values)
      )
    }
    val r = run(q)
    println( r.string )
    println( r.prepareRow.data.toList)
  }

  def extensionMapProc() = {
    inline def q = quote {
      query[Person].filterByKeys(values)
    }
    val r = run(q)
    println( r.string )
    println( r.prepareRow.data.toList)
  }

  def extensionColumnsProc() = {
    val columns = List("firstName")
    inline def q = quote {
      query[Person].filterColumns(columns)
    }
    // println(q)
    //val r = run(q) //hello
    // println( r.string )
    // println( r.prepareRow.data.toList)
  }

  /*
   ============= The following expansion happens ===========
   SELECT p.firstName, p.lastName, p.age FROM Person p
   WHERE
     ( p.firstName = [ values.getOrElse("firstName",null) ] OR [ values.getOrElse("firstName",null) == null ] ) AND
     ( p.lastName = [ values.getOrElse("lastName",null) ] OR [ values.getOrElse("lastName",null) == null ] ) AND
     ( p.age = [ values.getOrElse("age",null) ] OR [ values.getOrElse("age",null) == null ] ) AND true
  */

  def main(args: Array[String]): Unit = {
    regularMapProc()
    extensionMapProc()
  }

}
