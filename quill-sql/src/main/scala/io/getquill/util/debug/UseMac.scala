package io.getquill.util.debug

import io.getquill._
import io.getquill.Dsl._
import io.getquill.context.Context

case class Person(name: String, age: Int)

// trait SuperWithContext {
//   val ctx: Context[_, _]
//   import ctx._

//   val people = List(Person("Joe", 123))

//   inline def subField = quote { query[Person] }
//   inline def subFieldMethod = quote { (q: Query[Person]) => q.filter(p => p.name == "Joe") }
//   inline def subFieldInsert = quote { (p: Person) => query[Person].insert(p) }
// }

// class ExtOfSuperWithContext extends SuperWithContext {
//   val ctx = new MirrorContext(PostgresDialect, Literal)
//   import ctx._

//   inline def calling = quote { subFieldMethod(query[Person]) }
//   inline def batch = quote { liftQuery(people).foreach(p => subFieldInsert(p)) }

//   //def doRun = run(new ExtOfSuperWithContext().batch)
//   //def doPrint = PrintMac(liftQuery(people).foreach(p => subFieldInsert(p)))
//   def doPrint = PrintMac(new ExtOfSuperWithContext().batch)
// }

object UseMac {
  
  // val ctx = new MirrorContext(PostgresDialect, Literal)
  // import ctx._

  // val people = List(Person("Joe", 123))

  // trait Sub {
  //   inline def subField = quote { query[Person] }
  //   inline def subFieldMethod = quote { (q: Query[Person]) => q.filter(p => p.name == "Joe") }
  //   inline def subFieldInsert = quote { (p: Person) => query[Person].insert(p) }
  // }

  // class Super extends Sub {
  //   inline def calling = quote { subFieldMethod(query[Person]) }
  //   inline def batch = quote { liftQuery(people).foreach(p => subFieldInsert(p)) }
  // }

  case class Person(name: String, age: Int)

  def main(args: Array[String]):Unit = {

    import io.getquill.Ord

    val num = 4

    // PrintMac {
    //   -num
    // }

    // PrintMac {
    //   new Person("Joe", 123) //hello
    // }

    // PrintMac {
    //   new Tuple2("Joe", 123)
    // }

    // println( NewMac[Tuple2[String, Int]] )

    // println( NewMac[Person] )

    
    // NOTE doing querySchema here directly does not seem to work in InsertMacro, need to further test that
    //inline def q = quote { liftQuery(l).foreach(p => querySchema[Person]("thePerson").insert(p)) }

    // inline def q = quote { liftQuery(l).foreach(p => query[Person].insert(p)) }

    //inline def q = liftQuery(l).foreach(p => query[Person].insert(p))
    // import io.getquill.context.LiftMacro
    //inline def content = LiftMacro.liftInjectedProductExternal[Person, Int]
    //PrintMac(content)

    //val list = LiftMacro.liftInjectedProductExternal[Person, Int]
    //println( list.map(elem => (elem._1, elem._2.apply(Person("Joe", 123)))) )

    //PrintMac(q) //helloooooooooooooooooooooooooooooo

    // Uncomment to get error
    // println( run(q) )

    //run(new Super().batch)

    

  }
}