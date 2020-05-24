// package miniquill.parser

// import io.getquill.ast.{ Ident => Idnt, Constant => Const, Query => Qry, _}
// import miniquill.quoter._
// import scala.quoted._
// import scala.quoted.matching._
// import scala.annotation.StaticAnnotation
// import scala.deriving._
// import io.getquill.Embedable

// class MatroshkaHelper(using val qctx: QuoteContext) {
//   import qctx.tasty.{Term => QTerm, given _, _}

//   object TypedMatroshka {
//     // need to define a case where it won't go into matcher otherwise recursion is infinite
//     //@tailcall // should be tail recursive
//     def recurse(innerTerm: QTerm): QTerm = innerTerm match {
//       case Typed(innerTree, _) => recurse(innerTree)
//       case other => other
//     }

//     def unapply(term: QTerm): Option[QTerm] = term match {
//       case Typed(tree, _) => Some(recurse(tree))
//       case _ => None
//     }
//   }
// }

// class QuotationParser(using val qctx: QuoteContext) {
//   import qctx.tasty.{_, given _}
  
//   def unInline(expr: Expr[Any]): Expr[Any] = 
//     expr match {
//       // Need to traverse through this case if we want to be able to use inline parameter value
//       // without having to do quoted.unseal.underlyingArgument.seal
//       case Unseal(Inlined(_, _, v)) => unInline(v.seal)
//       case other => other
//     }

//   object Unseal {
//     def unapply(t: Expr[Any]) = {
//       Some(t.unseal)
//     }
//   }

//   import qctx.tasty.{_, given _}
//   import scala.quoted.matching.{Const => Constant} //hello

//   object TypedMatroshka {
//     // need to define a case where it won't go into matcher otherwise recursion is infinite
//     //@tailcall // should be tail recursive
//     def recurse(innerTerm: Term): Term = innerTerm match {
//       case Typed(innerTree, _) => recurse(innerTree)
//       case other => other
//     }

//     def unapply(term: Term): Option[Term] = term match {
//       case Typed(tree, _) => Some(recurse(tree))
//       case _ => None
//     }
//   }

//   object `Quoted.apply` {
//     def unapply(expr: Expr[Any]): Option[(Expr[Ast], Expr[List[Vase]], Expr[List[QuotationPouch]])] = expr match {
//       case '{ Quoted.apply[$qt]($ast, $v, $rq) } => 
//         //println("********************** MATCHED VASE INNER TREE **********************")
//         //printer.lnf(expr.unseal)
//         Some((ast, v, rq))
//       case Unseal(TypedMatroshka(tree)) => unapply(tree.seal)
//       case _ => 
//         //println("********************** NOT MATCHED VASE INNER TREE **********************")
//         //printer.lnf(expr.unseal)
//         None
//     }
//   }

//   protected object `QuotationLot.apply` {
//     def unapply(expr: Expr[Any]) = expr match {
//       case vase @ '{ QuotationLot.apply[$qt]($quotation, ${scala.quoted.matching.Const(uid: String)}) } => 
//         //println("********************** MATCHED VASE APPLY **********************")
//         //printer.lnf(expr.unseal)
//         Some((quotation, uid, vase))
//       case _ => None
//     }
//   }

//   object `ScalarVase.apply` {
//     def unapply(expr: Expr[Any]) = expr match {
//       case vase @ '{ ScalarVase.apply[$qt, $prep]($liftValue, $encoder, ${scala.quoted.matching.Const(uid: String)}) } =>
//         Some((liftValue, uid, vase, qt))
//       case _ => None
//     }
//   }

//   // Match the QuotationLot(...).unquote values which are tacked on to every
//   // child-quote (inside of a parent quote) when the 'unquote' function (i.e macro)
//   // is applied.
//   protected object `QuotationLot.unquote` {
//     def unapply(expr: Expr[Any]) = expr match {
//       // When a QuotationLot is embedded into an ast
//       case '{ (${quotationLot}: QuotationLot[$tt]).unquote } => Some(quotationLot)
//       case _ => None
//     }
//   }

//   protected object `(ScalarVase).unquote` {
//     def unapply(expr: Expr[Any]) = expr match {
//       case '{ (${vase}: ScalarVase[$tt, $pt]).unquote } => Some(vase)
//       case _ => None
//     }
//   }

//   object MatchRuntimeQuotationLots {
//     def unapply(expr: Expr[Any]): Option[(Expr[QuotationLot[Any]], String)] =
//       expr match {
//         // case MatchQuotationRef(tree, uuid) => 
//         //   println("******************** Runtime: Match Quotation Ref ********************")
//         //   printer.lnf((tree.unseal, uuid))
//         //   Some((tree, uuid))
//         case `QuotationLot.unquote`(innards) =>
//           //println("******************** Runtime: Match Unquote ********************")
//           //printer.lnf(innards.unseal)
//           unapply(innards)
//         // sometimes there are multiple levels of vases when references are spliced,
//         // we should only care about the innermost one
//         case `QuotationLot.apply`(_, uuid, vase) =>
//           //println("******************** Runtime: Vase Apply ********************")
//           //printer.lnf(uuid, vase)
//           Some((vase, uuid))
//         case _ => None
//       }
//     }

//   object MatchUprootableUnquote {
//     def unapply(expr: Expr[Any]): Option[(Expr[Ast], String)] =
//       expr match {
//         case `QuotationLot.unquote`(`QuotationLot.apply`(`Quoted.apply`((astTree, _)), uuid, _)) =>
//           Some((astTree, uuid))
//         case _ => None
//       }
//   }
// }
