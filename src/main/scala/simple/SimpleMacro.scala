package simple

import scala.quoted.{_}
import scala.annotation.StaticAnnotation
import printer.AstPrinter._
import printer.ContextAstPrinter._
import scala.deriving._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom}

case class Person(name: String, age:Int)

object SimpleMacro {

  trait Fooify[T] with
    def fooify:String
  
  // given Fooify[Long] = new Fooify[Long] with
  //     def fooify: String = "LongFoo"

  given Fooify[Int] = new Fooify[Int] with
    def fooify: String = "IntFoo"

  given Fooify[String] = new Fooify[String] with
    def fooify: String = "StringFoo"

  inline def processType[Elems <: Tuple]: List[String] =
    inline erasedValue[Elems] match {
      // case _: (String *: rest) =>
      //   val innerMirror =
      //     summonFrom {
      //       case m: Mirror.Of[elem] =>
      //         m match {
      //           case mp: Mirror.ProductOf[elem] => Some(mp)
      //           case _ => None
      //         }
      //       case _ => None
      //     }
      //   innerMirror match {
      //     case Some(m) => processType[m.MirroredElemTypes] ++ processType[rest]
      //     case None => List("string") ++ processType[rest]
      //       // summonFrom {
      //       //   case fe: Fooify[elem] => List(fe.fooify) ++ processType[rest]
      //       // }
      //   }

        

      case _: (elem *: rest) =>
        val innerMirror =
          summonFrom {
            case m: Mirror.Of[elem] =>
              m match {
                case mp: Mirror.ProductOf[elem] => Some(mp)
                case _ => None
              }
            case _ => None
          }
        innerMirror match {
          case Some(m) => processType[m.MirroredElemTypes] ++ processType[rest]
            summonFrom {
              case fe: Fooify[elem] => List(fe.fooify) ++ processType[rest]
            }
        }

      case _ => Nil
    }

  inline def fooifyType[T]: List[String] = {
    summonFrom {
      // it's a container type
      case m: Mirror.Of[T] =>
        println("Product mirror found: " + m)
        m match {
          case mp: Mirror.ProductOf[T] => processType[m.MirroredElemTypes]
        }
      // it's a single-value type
      case _ =>
        summonFrom {
          case fe: Fooify[elem] => List(fe.fooify)
        }
    }
  }
    

  inline def summonMirror[T]: Option[Mirror.ProductOf[T]] = {
    summonFrom {
      case m: Mirror.Of[T] =>
        m match {
          case mp: Mirror.ProductOf[T] => Some(mp)
          case _ => None
        }
      case _ => None
    }
  }

  inline def typeInfo[T](stuff: =>T): T = ${ typeInfoImpl('stuff) }
  def typeInfoImpl[T](stuff: Expr[T])(given qctx: QuoteContext, t: Type[T]): Expr[T] = {
    import qctx.tasty.{Type => TType, _, given}
    
    //  summon[scala.quoted.Type[T]].unseal.tpe
    //def getExprType[T <: Mirror.Of[_]](expr: Expr[T])(t: Type[T]) = 
     
    summonExpr[Mirror.Of[T]] match {
      case Some(expr) => println(expr.unseal.tpe.show)
      case None => println("Mirror not found")
    }
    
    // summon[Mirror.Of[T]] match {
    //   case m: Mirror.ProductOf[T] => println(m)
    // }
    
    stuff
  }


  // inline def printThenRun[T](print: String, thenRun: => T): T = ${ printThenRunImpl('print, 'thenRun) }
  // def printThenRunImpl[T](print: Expr[String], thenRun: Expr[T])(given qctx: QuoteContext) = {
  //   import qctx.tasty.{_, given} //Type => _,
    
  //   print.unseal.underlyingArgument match {
  //     case Literal(Constant(value)) => println(value)
  //     case _ => throw new RuntimeException("Needs a literal to be passed to the print method")
  //   }
  //   val output = thenRun
  //   output
  // }

  //inline def printThenRun[T](print: String, thenRun: => T): T = {
  //  println(print)
  //  thenRun
  //}oo

  // import dotty.tools.dotc.core.tasty.TastyPrinter

  // inline def betaReduceMethod(f: Int => Int ):Unit = ${betaReduceMethodImpl('f)}
  // def betaReduceMethodImpl(f: Expr[Int => Int])(given qctx: QuoteContext): Expr[Int] = {
  //   import qctx.tasty.{_, given}

  //   val reduced = Expr.betaReduce(f)('{123}) //hello
  //   println(astprint(reduced.unseal.underlyingArgument))
  //   println(reduced.show)
  //   reduced
  // }
    

  // //   '{()}
  // // }

  // inline def stuff[T](str: T):T = ${ stuffImpl('str) }
  // def stuffImpl[T](str: Expr[T])(given qctx: QuoteContext): Expr[T] = {
  //   import qctx.tasty.{_, given} //Type => _, 
  //   val und = str.unseal.underlyingArgument


  //   def splitPrint(str: String) = {
  //     val result = 
  //       str.split("\n").foldLeft(("--> ", List[String]())) { 
  //         case ((str, list), op) => ("    ", (str + op) +: list) 
  //       }._2.reverse.mkString("\n")
  //     println(result)
  //   }

  //   splitPrint(contextAstPrinter.apply(und).render)  //.showExtractors
  //   println(str.unseal.underlyingArgument.show)
  //   //TastyPrinter()
    
  //   str
  // }
}
