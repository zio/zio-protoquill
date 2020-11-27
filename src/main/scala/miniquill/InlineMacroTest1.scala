package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object NootExample {

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
}

object InlineMacroTest1 {
  // import scala.deriving._
  // import scala.compiletime.{erasedValue, constValue}
  // inline def oneOf[T <: Tuple, Orig <: NonEmptyTuple](inline tup: Orig, inline column:String, inline index:Int): Boolean =
  //   inline erasedValue[T] match {
  //     case _:(head *: tail) => column == "brrr" || oneOf[tail, Orig](tup, column, index + 1)
  //     case _:EmptyTuple => column == "barrrr"
  //   }

  import io.getquill._
  case class Address(street: String, zip: Int) extends Embedded
  given Embedable[Address]
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  inline def oneOf(inline list: List[String], inline column:String): Boolean = {
    inline if (ListProc.isNil(list))
      false
    else
      ListProc.index(list, 0) == column || oneOf(ListProc.tail(list), column)
  }

  case class Person(name: String, age: Int)

  inline def q = quote {
    query[Person].filter(p => oneOf(List("Joe", "Jack"), p.name))
  }
  println( run(q) )

  case class Node(status: String, lastStatus: String, backupStatus: String)

  inline def q1 = quote {
    query[Node].filter(n => oneOf(List(n.lastStatus, n.backupStatus), n.status))
  }
  println( run(q1) )

  inline def q2 = quote {
    query[Node].filter(n => oneOf(List(n.lastStatus, "restarting"), n.status))
  }
  println( run(q2) )

  def main(args: Array[String]): Unit = {
    
  }
}
