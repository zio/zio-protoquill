package derivation

import scala.deriving._
import Autogiven._
import Eq._

case class Blah(i: Int)

object AutogivenTest {
  def main(args: Array[String]):Unit = {
    //val intMirror = summon[Mirror.Of[Blah]]
    //println( Eq.derived(intMirror).eql(Blah(1), Blah(2)) )

    //println( Eq.derived(intMirror).eql(Person("Joe", 1), Person("Jack", 2)) )
    // println( autogiveEq(1, 2) )

    // This works... yay!
    autogiveEq(Blah(1), Blah(2))

    // Why can't I get the mirror of an int?
    //val intMirror = summon[Mirror.Of[Int]]
  }
}

