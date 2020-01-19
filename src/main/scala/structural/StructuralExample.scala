package structural

object StructuralExample {

  case class Record(elems: (String, Any)*) extends Selectable {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
  }

  type Person = Record {
    val name: String
    val age: Int
  }

  def main(args: Array[String]): Unit = {
    val person = Record("name" -> "Emma", "age" -> 42).asInstanceOf[Person]
    println(s"${person.name} is ${person.age} years old.")
    // Prints: Emma is 42 years old.
  }
}