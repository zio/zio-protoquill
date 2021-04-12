// package io.getquill.util.debug

// import scala.quoted._
// import io.getquill.metaprog.Extractors

// object TestMac {
//   inline def flattenOpt(inline list: List[Any]):List[Any] = ${ flattenOptImpl('list) }
//   def flattenOptImpl(list: Expr[List[Any]])(using qctx: Quotes): Expr[List[Any]] = {
//     import qctx.reflect._

//     def flattenOptions(expr: Expr[_]): Expr[_] = {
//       expr.asTerm.tpe.asType match {
//         case '[Option[Option[t]]] => 
//           //println(s"======= YES Flattening for ${Printer.TreeShortCode.show(expr.asTerm)}} *** ${Printer.TypeReprShortCode.show(expr.asTerm.tpe)} =======")
//           flattenOptions('{ ${expr.asExprOf[Option[Option[t]]]}.flatten })
//         // case '[Some[Some[t]]] => 
//         //   println(s"======= YES Flattening for ${expr.show} =======")
//         //   flattenOptions('{ ${expr.asExprOf[Option[Option[Any]]]}.flatten })
//         case _ =>
//           //println(s"------ NO Flattening for ${Printer.TreeShortCode.show(expr.asTerm)} *** ${Printer.TypeReprShortCode.show(expr.asTerm.tpe)} ------")
//           expr
//       }    
//     }

//     val elems = list match {
//       case '{ List.apply(${Varargs(elems)}: _*) } => elems
//     }

//     Expr.ofList(elems.map(flattenOptions).toList)
//   }
    

    
// }