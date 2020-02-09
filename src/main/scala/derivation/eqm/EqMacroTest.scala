package derivation.eqm

import Eq._
import EqMacro.{_, given _}

@main def testEqm() = {
  case class Foo(value: String)
  case class Inner(blah: String)
  case class Outer(i: Int, inner: Inner)

  println( Foo("one") === Foo("one") )
  println( Outer(1, Inner("blah")) === Outer(1, Inner("blah")) )
  println("one" == "one")
}