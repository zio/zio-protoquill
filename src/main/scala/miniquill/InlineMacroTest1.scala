
// Has "position not set" error
// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.Dsl._

// object InlineMacroTest1 {

//   // Can't do it like this because list is a runtime value
//   // inline def isOneOf(inline list: List[String], inline column:String): Boolean =
//   //   inline list match {
//   //     case leaf :: Nil => column == leaf
//   //     case head :: tail => column == head || isOneOf(tail, column)
//   //   }

//   trait Noot
//   case object Tip extends Noot
//   case class Node[N <: Noot](v: String, tail:N) extends Noot

//   // inline def nootOf[T <: Noot](inline noot: T, inline curr:String, inline column:String): Boolean =
//   //    inline noot match {
//   //      case n: Node[sub] => column == curr || nootOf(n.tail, n.v, column)
//   //      case Tip => false
//   //    }

//   // import scala.compiletime.erasedValue
//   // transparent inline def nootOf[T <: Noot](inline noot: T, inline column:String): Boolean =
//   //   inline erasedValue[T] match {
//   //     case Node(str, tail) => column == str || nootOf(tail, column)
//   //     case Tip => false
//   //   }

//   // import scala.deriving._
//   // import scala.compiletime.{erasedValue, constValue}
//   // inline def oneOf[T <: Tuple, Orig <: NonEmptyTuple](inline tup: Orig, inline column:String, inline index:Int): Boolean =
//   //   inline erasedValue[T] match {
//   //     case _:(head *: tail) => column == "brrr" || oneOf[tail, Orig](tup, column, index + 1)
//   //     case _:EmptyTuple => column == "barrrr"
//   //   }


//   inline def oneOf(inline list: List[String], inline column:String): Boolean = {
//     inline if (ListProc.isNil(list))
//       false
//     else
//       ListProc.index(list, 0) == column || oneOf(ListProc.tail(list), column)
//   }

//   def main(args: Array[String]): Unit = {

//     //hello
//     import io.getquill._
//     case class Address(street: String, zip: Int) extends Embedded //helloooo
//     given Embedable[Address] //hello
//     case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)

//     // PriceIncrement()
//     // Strike(price: )
//     //inline def personToField(inline p: Person) = p.name

//     inline def q = quote { //hello
//       // pattern is too complex
//       //query[Person].filter(p => nootOf(Node("foo", Node("bar", Tip)), p.name))
//       //query[Person].filter(p => oneOf[String *: EmptyTuple.type, String *: EmptyTuple.type](("foo" *: EmptyTuple), "blah", 0))
//       // This works!!!
//       query[Person].filter(p => oneOf(List(p.middleName,p.lastName), p.name))
//       //query[Person]
//     }

//     val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//     import ctx._
    
//     //inline def q = quote { query[Person].union(query[Person]) } //helloooooooo

    
//     val output = run(q)
//     println(output)
//   }
// }
