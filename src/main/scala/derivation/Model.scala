package derivation

case class ThePerson(name:String, age:Int, addresses: List[TheAddress])
case class TheAddress(street: String)


case class PersonSimple(name:String, age:Int)
object Simple {
  val stuff = PersonSimple("Joe", 123)
}

