// //TODO Rewrite this test with eager encodeables

// package miniquill.quoter

// import miniquill.quoter.ScalarPlanter
// import miniquill.quoter.ScalarPlanterExpr
// import scala.quoted._
// import miniquill.dsl.GenericEncoder
// import  //Expr.summon also from here
// //import scala.compiletime.Expr.summon

// case class LazyPlanter[T](uid: String, value: T)


// // Need to create a example user macro of FindEncodeables to be able to test it
// object FindEncodeablesUserMacro {
//   // Note, existential typing here is better. Otherwise everything is forced to cast to Any and the encoder lookup does not work
//   inline def apply[PrepareRow](inline encodeables: List[LazyPlanter[_]], inline prep: PrepareRow): List[(Any, PrepareRow)] = ${ applyImpl[PrepareRow]('encodeables, 'prep) }
//   def applyImpl[PrepareRow](encodeables: Expr[List[LazyPlanter[_]]], prep: Expr[PrepareRow])(using qctx: QuoteContext, pType: Type[PrepareRow]): Expr[List[(Any, PrepareRow)]] = {
//     import qctx.reflect._

//     object InlinePlanter {
//       def unapply(expr: Expr[LazyPlanter[_]]): Option[ScalarPlanterExpr[_, PrepareRow]] = expr match {
//         case vvv @ '{ LazyPlanter[$tt](${Const(uid: String)}, $value) } =>
//           val planterType = tt
//           val encoder = 
//             Expr.summon(using '[GenericEncoder[$tt, $pType]]) match {
//               case Some(enc) => Option(ScalarPlanterExpr(uid, value, enc)(planterType, pType))
//               case None => report.throwError(s"Cannot Find encode for ${tt.unseal}", vvv)
//               // TODO return summoning error if not correct
//             }
//           encoder
//       }
//     }

//     object PlanterList {
//       def unapply(expr: Expr[List[LazyPlanter[_]]]): Option[List[ScalarPlanterExpr[_, PrepareRow]]] = expr match {
//         case '{ scala.List[$t](${GenericSeq(elems)}: _*) } => 
//           val planters =
//             elems.toList.map {
//               case InlinePlanter(planterExpr) => planterExpr
//             }
//           Some(planters)
//       }
//     }

//     val foundEncodeables: List[ScalarPlanterExpr[Any, PrepareRow]] = encodeables match {
//       case PlanterList(enc) => enc.asInstanceOf[List[ScalarPlanterExpr[Any, PrepareRow]]]
//     }

//     val encoded =
//       foundEncodeables.zipWithIndex.map((enc, idx) => {
//         '{ (${enc.expr}, ${enc.encoder}.apply(${Expr(idx)}, ${enc.expr}, ${prep}).asInstanceOf[PrepareRow]) }
//       })

//     Expr.ofList(encoded)
//   }

// }