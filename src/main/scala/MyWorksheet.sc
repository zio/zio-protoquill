
// type MyType = String | List[String]
// val l: MyType = List("foo", "bar")

// val v = l match {
//     case _:String => "string"
//     case _:List[String] => "string list"
// }

// v

// import Miniquill._

// case class Person(id: Int, name: String, age: Int)

// val areas = quote {
//   query[Person].map((p: Person) => p.name) //: EntityQuery[Double]
// }



// run(areas)

import scala.quoted.{given, _}

// def showExpr[T](expr: Expr[T]): Expr[String] = {
//   val code: String = expr.show
//   Expr(code)
// }

