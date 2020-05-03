// package simple

// import scala.quoted._
// import scala.quoted.matching.{Const => ConstExpr, _} // Including ExprSeq?
// import miniquill.quoter.{QuoteMeta, Dsl}
// import miniquill.parser.TastyMatchersContext

// object MacroExperiment {
//   case class Foo(value: String)

//   inline def stuff: List[Any] = List.apply[Any](Foo("blah"), 3, 4.5, Foo("blah blah"))
//   def reg_stuff: List[Any] = List.apply[Any](Foo("blah"), 3, 4.5, Foo("blah blah"))                                      

//   inline def printTree(tree: Any): Any = ${ printTreeImpl('tree) }
//   def printTreeImpl(tree: Expr[Any])(given qctx: QuoteContext): Expr[Any] = {
//     import qctx.tasty.{given _, _}

//     // tree.unseal.underlyingArgument.seal match {
//     //   case '{ type $tt; FunObject.fun[`$tt`](($arg: `$tt`) => ($out: String)) } => "blah"
//     // }
//     val resealed = tree.unseal.underlyingArgument.seal

//     val su = new TastyMatchersContext
//     import su._

//     resealed match {
//       // Step 1 - try to reproeduce the initial expression
//       // case '{ ($x1: $tpe1) => ArrowAssoc[$t]($prop).->[$v](${ConstExpr(alias: String)}) } =>
//       //   println("============= " + prop.show + " : " + alias + "==============")

//       // Step 2 - Is the problem with the ConstExpr???... still happens
//       // case '{ type $tt; ($x1: $tpe1) => ArrowAssoc[`$tt`]($prop: `$tt`).->[$v]($inside) } => //${ConstExpr(alias: String)}
//       //   println("============= " + prop.show + " : " + inside.show + "==============")

//       // Step 3- Maybe it's with the whole arrow function?... still happens
//       // case '{ ($x1: $tpe1) => ($out: $tpe2) } => //${ConstExpr(alias: String)}
//       //  println("============= " + out.show + " ==============")

//       // Step 4 - What if we type (i.e. hard-code) the first argument ... still happens
//       // case '{ ($x1: String) => ($out: $tpe2) } => //${ConstExpr(alias: String)}
//       //  println("============= " + out.show + " ==============")

//       // Step 5 - What if we hard-code both types?
//       // case '{ ($x1: String) => ($out: String) } => //${ConstExpr(alias: String)}
//       //  println("============= " + out.show + " ==============")
//       // So this, works, and the problem is using a type ascription inside of a closure doesn't work!

//       //Step 6 Part1 - Let's try to parse the entire quote meta:
//       case vv @ '{ ($qm: QuoteMeta[$qt]).querySchema[$t]($foo, ${ExprSeq(props)}: _*) } => // back here

//         def parsePropertyAlias(prop: Expr[_]): String = {
//           prop match {
//             // Causes Hang
//             //case vvv @ '{ ($x1: `$t`) => scala.Predef.ArrowAssoc[$tPA]($prop).->[$v](${ConstExpr(alias: String)}) } =>
//             case vvv @ Lambda1(_, av @ '{ ArrowAssoc[$tpa]($prop).->[$v](${ConstExpr(alias: String)}) } ) =>
//               def path(tree: Expr[_]): List[String] =
//                 tree match {
//                   // case q"$a.$b" =>
//                   case a`.`b => path(a) :+ b
//                   // case q"$a.$b.map[$tpe]((..$args) => $tr)" =>

//                   // This doesn't work, let's try this:
//                   //case '{ type $ot; ($b: Option[`$ot`]).map[$tpe](($arg: `$ot`) => $tr) } =>

//                   // This doesn't work, let's try this
//                   //case SelectApply1(SelectExpr(a, b), "map", Lambda1(arg, body)) => 
//                   //  path(a) ++ (b :: path(body))

//                   // This seems to work
//                   case '{ (${a`.`b}: Option[$t]).map[$r](${Lambda1(arg, body)}) } =>

//                   //case '{ type $t; (${SelectExpr(a, b)}: Option[`$t`]).map[$r]($f) } =>

//                     path(a) ++ (b :: path(body))

//                   //case '{ type $tt; (${SelectExpr(a, b)}: Option[`$tt`]).map[$rr](($arg: `$tt`) => $body) } =>
//                   // List("foo")

//                   // TODO Something more generic that covers SelectApply as well as just Select1?

//                   case other => 
//                     printer.lnf(other.unseal)
//                     println(other.show)
//                     Nil
//                 }

//               println("======= Matched! Parser Alias ======")
//               println(av.show)

//               println("====== Path =====")
//               println(path(prop))

//               //alias
//               "foo"
//           }
//         }

//          println("======= Matched! ======")
//          println(vv.show)

//          props.map(parsePropertyAlias(_))
        
//         // println(bar.show)
//         // println(baz.show)

//       //case '{ ($s: Shell).colVar[$t]($one, $v) } => println(v.show) //hello

//       //case vv @ '{ ($s: Shell[$tt]).colVar[$t]($one, ${ExprSeq(props)}: _*) } => println(vv.show) //hello

//       //case vv @ '{ ($s: Shell[$tt]).colVar[$t]($one) } => println(vv.show) //hello

//       //case vv @ '{ ($s: Shell[$tt]).colVar($one) } => println(vv.show) //hello

//       // case vv @ '{ ($s: Shell).colVar($one) } => println(vv.show) //hello

//       case _ =>
//         println("=============== NOT Matched =============")
//         println(resealed.show)
//     }

//     //println(tree.unseal.underlyingArgument.seal.show)
//     //printer.lnf(tree.unseal.underlyingArgument)
//     tree
//   }

// }

// // object ShellObj extends Shell[String]
// // trait Shell[TT] {
// //   //def colVar[T](first: String, columns: (T => (Any, String))*): String = "blah"
// //   //def colVar[T](first: String): String = "blah"
// //   def colVar(first: String): String = "blah"
// // }

// object ShellObj extends Shell
// trait Shell {
//   //def colVar[T](first: String, columns: (T => (Any, String))*): String = "blah"
//   //def colVar[T](first: String): String = "blah"
//   def colVar(first: String): String = "blah"
// }