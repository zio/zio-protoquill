package io.getquill

import io.getquill.quoter.Dsl._
import io.getquill.quoter.Dsl.autoQuote
import io.getquill.parser._

object ActionParserTest {
  def main(args: Array[String]):Unit = {
    println('\n'*10)

    case class Person(name:String, age:Int)

    val insertOutput = quote {
      query[Person].insert(_.name -> "John", _.age -> 21)
      //INSERT INTO Person (name,age) VALUES (?, ?)
    }
    run(insertOutput)

    val updateOutput = quote {
      query[Person].filter(_.name=="Joe").update(_.name -> "John")
       //UPDATE Person SET name = ? WHERE name = ?
    }
    run(updateOutput)

    inline def deleteOutput = quote {
      query[Person].filter(p => p.name == "Joe").delete
      //DELTE FROM Person WHERE name = 'Joe'
    }
    run(deleteOutput)

    // val queryReturningOutput = quote {
    //   query[Person].insert(_.name -> "John", _.age -> 21).returning(p => p.name)
    //   //INSERT INTO Person (name, age) VALUES ('Joe', 21) RETURNING name
    // }
    // run(queryReturningOutput)

    // val queryReturningGeneratedOutput = quote {
    //   query[Person].insert(_.name -> "John", _.age -> 21).returningGenerated(p => p.name)
    //   //INSERT INTO Person (id, name, age) VALUES (-1, 'Joe', 1) RETURNING id
    // }
    // run(queryReturningGeneratedOutput)
    
    //compileeeeeeeee

    println('\n'*10)
  }
}