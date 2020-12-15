// package derivation

// import scala.quoted._
// //
// import scala.compiletime.{erasedValue, summonFrom}
// //import dotty.tools.dotc.ast.untpd
// //import dotty.tools.dotc.core.Names._

// object Retuplify {
//   inline def retuplify[T](tuple: Tuple, newValue: T): Tuple = ${ retuplifyImpl('tuple, 'newValue) }
//   def retuplifyImpl[T:Type](tuple: Expr[Tuple], newValue: Expr[T])(using Quotes): Expr[Tuple] = {
//     import quotes.reflect._
  
//     printer.ln(Term.of(tuple).underlyingArgument)

//     // tuple match {
//     //   case '{ ($elem *: ($elems1: $tpe)) } => println(s"ELEM: $elem ELEMS: $elems1")
//     // }

//     '{null}
//   }
// }

// object ChangeFoosToBars {
//   inline def changeFoos(block: =>String): String = ${ changeFoosImpl('block) }
//   def changeFoosImpl(block: Expr[String])(using Quotes): Expr[String] = {
//     import quotes.reflect._
//     //printer.ln(Term.of(block))

//     object Unseal {
//       def unapply(t: Expr[Any])(using Quotes) = {
//         Some(Term.of(t))
//       }
//     }
//     object Seal {
//       def unapply[T](e: Term)(using Quotes) = { // TODO: This cast is unsound. Use `e match { case '{ $e: t } => ... }`
//         implicit val ttpe: quoted.Type[T] = e.tpe.asType.asInstanceOf[quoted.Type[T]]
//         Some(e.asExprOf[T])
//       }
//     }

//     object rewriter extends ExprMap {
//       def transform[T](e: Expr[T])(using Quotes, Type[T]): Expr[T] = e match {
//         //case '{ (${Unseal(Ident("foo"))}: String) } => 
//         //  '{ "blahblah" }.cast[T]
//         case _ => transformChildren(e)
//       }
//     }

//     rewriter.transform[String](block)
//   }
// }


