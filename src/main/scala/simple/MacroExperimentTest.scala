package simple

import miniquill.quoter.Dsl
import miniquill.quoter._
import simple.MacroExperiment._
import miniquill.quoter.Dsl._

@main def testOtherstuff() = { //hello
  import miniquill.quoter.Dsl

  //inline def fun[T,R](inline funfun: Function[T,R]): Function[T,R] = funfun
  val fun = (str: String) => str.length

  case class Person(name: String, age: Int)

  // printTree {
  //   ((q: Query[Person]) => q.map(p => p.name)).apply(EntityQuery[Person])
  // }
}
