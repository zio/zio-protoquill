// //TODO Rewrite this test with eager encodeables

// package miniquill.quoter

// import miniquill.quoter.ScalarPlanter
// import miniquill.quoter.ScalarPlanterExpr
// import scala.quoted._
// import miniquill.dsl.GenericEncoder
// import scala.quoted.matching._ //summonExpr also from here
// //import scala.compiletime.summonExpr

// case class LazyPlanter[T](uid: String, value: T)


// // Need to create a example user macro of FindEncodeables to be able to test it
// object FindEncodeablesUserMacro {
//   // Note, existential typing here is better. Otherwise everything is forced to cast to Any and the encoder lookup does not work
//   inline def apply[PrepareRow](inline encodeables: List[LazyPlanter[_]], inline prep: PrepareRow): List[(Any, PrepareRow)] = ${ applyImpl[PrepareRow]('encodeables, 'prep) }
//   def applyImpl[PrepareRow](encodeables: Expr[List[LazyPlanter[_]]], prep: Expr[PrepareRow])(given qctx: QuoteContext, pType: Type[PrepareRow]): Expr[List[(Any, PrepareRow)]] = {
//     import qctx.tasty.{given, _}

//     object Inline {
//       def unapply(expr: Expr[Any]): Option[ScalarPlanterExpr[_, _]] = expr match {
//         case vvv @ '{ LazyPlanter[$tt](${Const(uid: String)}, $value) } =>
//           val encoder = 
//             summonExpr(given '[GenericEncoder[$tt, $pType]]) match {
//               case Some(enc) => Some(ScalarPlanterExpr(uid, value, enc))
//               case None => qctx.throwError(s"Cannot Find encode for ${tt.unseal}", vvv)
//               // TODO return summoning error if not correct
//             }
//           encoder
//       }
//     }

//     object List {
//       def unapply(expr: Expr[Any]): Option[List[ScalarPlanterExpr[_, _]]] = expr match {
//         case '{ scala.List[$t](${ExprSeq(elems)}: _*) } => 
//           val planters =
//             elems.toList.map {
//               case Inline(planterExpr) => planterExpr
//             }
//           Some(planters)
//       }
//     }

//     val foundEncodeables: List[ScalarPlanterExpr[_, _]] = encodeables match {
//       case List(enc) => enc
//     }

//     val encoded =
//       foundEncodeables.zipWithIndex.map((enc, idx) => {
//         '{ (${enc.expr}, ${enc.encoder}.apply(${Expr(idx)}, ${enc.expr}, ${prep}).asInstanceOf[PrepareRow]) }
//       })

//     Expr.ofList(encoded)
//   }

// }