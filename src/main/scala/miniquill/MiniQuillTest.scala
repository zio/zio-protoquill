package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.parser.PrintMac
import miniquill.parser.MatchMac
import miniquill.parser.MatchLambdaMac

object AdvTest {

  // Can't do it like this because list is a runtime value
  // inline def isOneOf(inline list: List[String], inline column:String): Boolean =
  //   inline list match {
  //     case leaf :: Nil => column == leaf
  //     case head :: tail => column == head || isOneOf(tail, column)
  //   }

  trait Noot
  case object Tip extends Noot
  case class Node[N <: Noot](v: String, tail:N) extends Noot

  // inline def nootOf[T <: Noot](inline noot: T, inline curr:String, inline column:String): Boolean =
  //    inline noot match {
  //      case n: Node[sub] => column == curr || nootOf(n.tail, n.v, column)
  //      case Tip => false
  //    }

  // import scala.compiletime.erasedValue
  // transparent inline def nootOf[T <: Noot](inline noot: T, inline column:String): Boolean =
  //   inline erasedValue[T] match {
  //     case Node(str, tail) => column == str || nootOf(tail, column)
  //     case Tip => false
  //   }

  // import scala.deriving._
  // import scala.compiletime.{erasedValue, constValue}
  // inline def oneOf[T <: Tuple, Orig <: NonEmptyTuple](inline tup: Orig, inline column:String, inline index:Int): Boolean =
  //   inline erasedValue[T] match {
  //     case _:(head *: tail) => column == "brrr" || oneOf[tail, Orig](tup, column, index + 1)
  //     case _:EmptyTuple => column == "barrrr"
  //   }


  inline def oneOf(inline list: List[String], inline column:String): Boolean = {
    inline if (ListProc.isNil(list))
      false
    else
      ListProc.index(list, 0) == column || oneOf(ListProc.tail(list), column)
  }

  def takeLambda(f: String => Int): Unit = ()

  def main(args: Array[String]): Unit = {

    //hello
    import io.getquill._
    case class Address(street: String, zip: Int) extends Embedded //helloooo
    given Embedable[Address] //hello
    case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)

    // PriceIncrement()
    // Strike(price: )
    //inline def personToField(inline p: Person) = p.name

    inline def q = query[Person].insert(p => p.name -> "Joe")
    
    PrintMac(q)
    MatchMac(q)

    inline def lambdaExample = (p: String) => p.length
    PrintMac(lambdaExample)
    MatchLambdaMac(lambdaExample)
    

    // inline def q = quote {
    //   //query[Person].map(p => p.age / 4) //helloooooooooooooooooooooooooooooooooooooooooooooooooooo
    //   query[Person].insert(_.name -> "Joe")
    // }

    // val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    // import ctx._
    
    // val output = run(q)
    // println(output)
  }
}
