package simple

import scala.quoted._
import printer.AstPrinter

// TODO Can I compose two inline partial functions (using orElse or something like that)?

// case class CaseClassMacro(int: Int) {
//   inline def stuff[T](doStuff: =>T): T = ${ stuffImpl('doStuff) }
//   def stuffImpl[T](doStuff: Expr[T])(using Quotes): Expr[T] = {
//     println(new AstPrinter().apply(stuff))
//     doStuff
//   }
// }