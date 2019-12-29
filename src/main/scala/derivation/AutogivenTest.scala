package derivation

import scala.deriving._
import Autogiven._
import Eq._



object AutogivenTest {
  val stuff = ThePerson("Joe", 123, List(TheAddress("123 Street")))

  def main(args: Array[String]):Unit = {
    
    //println( Eq.derived(intMirror).eql(Person("Joe", 1), Person("Jack", 2)) )
    // println( autogiveEq(1, 2) )

    // This works... yay! ... but without Address also being there!
    //println( autogiveJsonEncoder[ThePerson](stuff) )

    // Why can't I get the mirror of an int?
    //val intMirror = summon[Mirror.Of[Int]

    // given JsonEncoder[ThePerson] = JsonEncoder.derived
    // given JsonEncoder[TheAddress] = JsonEncoder.derived
    // println( summon[JsonEncoder[ThePerson]].encode(stuff) )
  }
}
