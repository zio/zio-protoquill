// package io.getquill

// import scala.language.implicitConversions
// import io.getquill.quoter.Dsl._
// import io.getquill.quoter.Quoted
// import io.getquill.quoter._
// import io.getquill.ast._
// import io.getquill.quoter.QuotationLot
// import io.getquill.quoter.QuotationVase
// import io.getquill.context.ExecutionType

// class QueryTestMain {

//   case class Person(name: String, age: Int)
//   val ctx = new MirrorContext(MirrorIdiom, Literal)

//   def peopleRuntime = quote {
//     query[Person]
//   }
  

//   def main(args: Array[String]):Unit = {
//     import ctx._
//     val result = ctx.run(peopleRuntime.map(p => p.name + lift("hello")))
//     println( result.string == """querySchema("Person").map(p => p.name + ?)""" )
//     println( result.executionType == ExecutionType.Dynamic )
//   }

// }