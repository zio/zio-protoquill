package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.Planter
import io.getquill.ast.{ Ast, Ident => AIdent }
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.EagerPlanter
import io.getquill.LazyPlanter
import io.getquill.generic.GenericEncoder
import io.getquill.generic.ElaborateStructure
import io.getquill.quat.Quat
import scala.quoted._
import io.getquill._
import io.getquill.quat.QuatMaking
import io.getquill.generic.ElaborateStructure.TaggedLiftedCaseClass
import io.getquill.parser.Lifter
import io.getquill.CaseClassLift

object LiftQueryMacro {
  private[getquill] def newUuid = java.util.UUID.randomUUID().toString

  def apply[T: Type, U[_] <: Iterable[_]: Type, PrepareRow: Type](entity: Expr[U[T]])(using Quotes): Expr[Query[T]] = {
    import quotes.reflect._
    // check if T is a case-class (e.g. mirrored entity) or a leaf, probably best way to do that
    val quat = QuatMaking.ofType[T]
    quat match
      case _: Quat.Product => 
        // Not sure why cast back to iterable is needed here but U param is not needed once it is inside of the planter
        '{ EagerEntitiesPlanter($entity.asInstanceOf[Iterable[T]], ${Expr(newUuid)}).unquote } // [T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
      case _ => 
        report.throwError("Scalar liftQuery not implemented yet", entity)
  }
}

object LiftMacro {
  private[getquill] def newUuid = java.util.UUID.randomUUID().toString
  private[getquill] val VIdent = AIdent("v", Quat.Generic)

  def apply[T: Type, PrepareRow: Type](entity: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect._

    // check if T is a case-class (e.g. mirrored entity) or a leaf, probably best way to do that
    val quat = QuatMaking.ofType[T]
    quat match
      case _: Quat.Product => 
        '{ ${liftProduct[T, PrepareRow](entity)}.unquote }
      case _ => 
        var liftPlanter = liftValue[T, PrepareRow](entity)
        '{ $liftPlanter.unquote }
  }

  
  private[getquill] def liftProduct[T, PrepareRow](productEntity: Expr[T])(using qctx:Quotes, tpe: Type[T], prepareRowTpe: Type[PrepareRow]): Expr[CaseClassLift[T]] = {
    import qctx.reflect._
    val TaggedLiftedCaseClass(caseClassAst, lifts) = ElaborateStructure.ofProductValue[T](productEntity).reKeyWithUids()
    val liftPlanters = 
      lifts.map(
        (liftKey, lift) => 
          // since we don't have an implicit Type for every single lift, we need to pull out each of their TypeReprs convert them to Type and manually pass them in
          // Also need to widen the type otherwise for some value v=Person(name: String) the type will be TermRef(TermRef(NoPrefix,val v),val name) as oppsoed to 'String'
          val liftType = lift.asTerm.tpe.widen.asType
          liftType match {
            case '[liftT] =>
              liftValue[liftT, PrepareRow](lift.asExprOf[liftT], liftKey) // Note: if want to get this to work, try doing 'summon[Type[liftT]]' (using liftType, prepareRowTpe, quotes)
          }
      )
    val quotation = '{ Quoted[T](${Lifter(caseClassAst)}, ${Expr.ofList(liftPlanters)}, Nil) }
    '{ CaseClassLift[T]($quotation, ${Expr(java.util.UUID.randomUUID.toString)}) } // NOTE UUID technically not needed here. Can try to remove it later
  }

  private[getquill] def liftValue[T: Type, PrepareRow: Type](valueEntity: Expr[T], uuid: String = newUuid)(using Quotes) /*: Expr[EagerPlanter[T, PrepareRow]]*/ = {
    import quotes.reflect._
    val encoder = 
      Expr.summon[GenericEncoder[T, PrepareRow]] match
        case Some(enc) => enc
        case None => report.throwError(s"Cannot Find a '${Printer.TypeReprCode.show(TypeRepr.of[T])}' Encoder of ${Printer.TreeShortCode.show(valueEntity.asTerm)}", valueEntity)

    '{ EagerPlanter($valueEntity, $encoder, ${Expr(uuid)}) } //[T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }

  def applyLazy[T, PrepareRow](valueEntity: Expr[T])(using Quotes, Type[T], Type[PrepareRow]): Expr[T] = {
    import quotes.reflect._
    val uuid = java.util.UUID.randomUUID().toString
    '{ LazyPlanter($valueEntity, ${Expr(uuid)}).unquote } //[T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }
}
