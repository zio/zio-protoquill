package derivation

import scala.deriving._
import Eq._

case class Person(name: String, age: Int)

case class Mono[T](t: T)

object EqTest {
  // using Eq[Person] = Eq.derived
  // def main(args: Array[String]):Unit = {
  //   val equfunc = summon[Eq[Person]]
  //   println(equfunc.eql(Person("Joe", 1), Person("Jack", 1)))
  // }

  // **********************************************************

    //using [T: Eq]: Eq[Mono[T]] = Eq.derived
  //implicit val personMirror: Mirror.Of[Person] = 
  //  summon[Mirror {type MirroredType = Person}]

  val s = summon[Mirror.Of[Person]]

  implicit val eqPerson: Eq[Person] = Eq.derived

  def main(args: Array[String]):Unit = {
    val equfunc = summon[Eq[Person]]
    println(equfunc.eql(Person("Joe", 1), Person("Jack", 1)))

    //println(
    //  Eq.derived(personMirror).eql(Person("Joe", 1), Person("Jack", 1))
    //)
    

  }
}