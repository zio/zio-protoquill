package derivation.eqm

import Labeled._
import LabeledMacro._

object LabelMacroTest {
  def main(args: Array[String]): Unit = {
    case class Foo(one: String, two: String)
    case class Bar(blah: String, fooMem: Foo)

    println( LabeledMacro.label[Bar] )

  }
}
