package deriveexample

@main def testDerive() = {
  case class Address(street: Int)
  case class Person(name: Int, address: Address)
  import deriveexample.AutomaticEq._

  // fooo
  println( summon[Eq[Person]].eqv(Person(1, Address(2)), Person(1, Address(2))) )
}