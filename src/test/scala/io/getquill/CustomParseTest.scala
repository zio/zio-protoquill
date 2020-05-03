// package io.getquill

// import scala.language.implicitConversions
// import miniquill.quoter._
// import io.getquill._
// import io.getquill.ast._
// import miniquill.quoter.Quoted
// import io.getquill._
// import org.scalatest._

// object CustomDsl extends QuoteMeta[CustomParserFactory]

// class CustomParseTest extends Spec with Inside {
//   val customDsl = CustomParserFactory
//   import customDsl._

//   case class Person(name: String, age: Int)
//   implicit class CustomOps(i: Int) {
//     def **(other: Int) = i * other
//   }

//   // TODO Write the actual matcher in CustomParserFactory that will match
//   // CustomOps ** operator and make it just do a regular * operator for the parser
//   "can customize parsing spec" in {

//   }

// }