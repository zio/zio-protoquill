package simple

import io.getquill.quoter.Dsl
import io.getquill.quoter._
import simple.MacroExperiment._
import io.getquill.quoter.Dsl._

@main def testOtherstuff() = { //hello
  import io.getquill.quoter.Dsl

  //inline def fun[T,R](inline funfun: Function[T,R]): Function[T,R] = funfun
  val fun = (str: String) => str.length

  case class Person(name: String, age: Int)

  // printTree {
  //   ((q: Query[Person]) => q.map(p => p.name)).apply(EntityQuery[Person])
  // }
}
