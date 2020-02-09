package derivation.eqm

import Eq._
import EqMacro.{_, given _}

@main def testEqm() = {
  case class Foo(value: String)
  Foo("one") === Foo("one")
}