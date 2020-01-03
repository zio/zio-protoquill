package scoping

import PulloutExperiment._

@main def pulloutTest() = {
  val v = "foo"
  inline def value = pullout(List(lookInside(v), "foo", lookInside(v)))
  printTree(value)

  val output = parseTuple(value)

  //println(output)

  //println(pullout( ("foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar", "foo", "bar") ))

}
