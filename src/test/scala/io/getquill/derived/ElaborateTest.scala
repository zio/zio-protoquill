package io.getquill.derived

object ElaborateTest {
  
  // def test1 = {
  //   case class Person(name: String, age: Int)
  //   val p = Person("Joe", 123)
  //   println( ElaborateMac.elaborate(p) ) //hello
  // }

  // def test15 = {
  //   // Parses like this: Term(x,Branch,List(Term(name,Branch,List(Term(value,Leaf,List(),false)),false), Term(age,Leaf,List(),false)),false)
  //   // that's wrong, should not be parsing this way. Should add test in ExpanderTests for this use case
  //   case class Person(name: Option[String], age: Int)
  //   val p = Person(Some("Joe"), 123)
  //   println( ElaborateMac.elaborate(p) )
  // }

  // def test2 = {
  //   case class Name(first: String, last: String)
  //   case class Person(name: Name, age: Int)
  //   val p = Person(Name("Joe", "Bloggs"), 123)
  //   println( ElaborateMac.elaborate(p) )
  // }

  // def test3 = {
  //   case class Name(first: String, last: String)
  //   case class Person(name: Option[Name], age: Int)
  //   val p = Person(Some(Name("Joe", "Bloggs")), 123)
  //   println( ElaborateMac.elaborate(p) )
  //   val p1 = Person(None, 123)
  //   println( ElaborateMac.elaborate(p) )
  // }

  def test4 = {
    case class Name(first: Option[String], last: String)
    case class Person(name: Option[Name], age: Int)
    val p = Person(Some(Name(Some("Joe"), "Bloggs")), 123)
    println( ElaborateMac.elaborate(p) )
    //val p1 = Person(None, 123)
    //println( ElaborateMac.elaborate(p) )
  }
  
  

  def main(args: Array[String]): Unit = {
    // test3
    test4
    //test15
  }
}