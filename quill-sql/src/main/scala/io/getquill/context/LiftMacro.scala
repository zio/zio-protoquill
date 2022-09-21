package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ReturnAction}
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.Planter
import io.getquill.ast.{Ast, Ident => AIdent}
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
import io.getquill.ast.CaseClass
import io.getquill.InjectableEagerPlanter
import io.getquill.util.Format
import io.getquill.metaprog.etc.StringOrNull
import scala.reflect.ClassTag

object LiftQueryMacro {
  private[getquill] def newUuid = java.util.UUID.randomUUID().toString

  def apply[T: Type, U[_] <: Iterable[_]: Type, PrepareRow: Type, Session: Type](entity: Expr[U[T]])(using Quotes): Expr[Query[T]] = {
    import quotes.reflect._
    // check if T is a case-class (e.g. mirrored entity) or a leaf, probably best way to do that
    val quat = QuatMaking.ofType[T]
    quat match
      case _: Quat.Product =>
        // Not sure why cast back to iterable is needed here but U param is not needed once it is inside of the planter
        val (lifterClass, lifters) =
          LiftMacro.liftInjectedProduct[T, PrepareRow, Session]
        val lifterClassExpr = Lifter.caseClass(lifterClass)
        val liftedLiftersExpr = Expr.ofList(lifters)
        val returning =
          '{ EagerEntitiesPlanter($entity.asInstanceOf[Iterable[T]], ${ Expr(newUuid) }, ${ liftedLiftersExpr }, ${ lifterClassExpr }).unquote }
        returning
      case _ =>
        val encoder = LiftMacro.summonEncoderOrFail[T, PrepareRow, Session](entity)
        // [T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
        '{ EagerListPlanter($entity.asInstanceOf[Iterable[T]].toList, $encoder, ${ Expr(newUuid) }).unquote }
  }
}

object LiftMacro {
  private[getquill] def newUuid = java.util.UUID.randomUUID().toString
  private[getquill] val VIdent = AIdent("v", Quat.Generic)

  def apply[T: Type, PrepareRow: Type, Session: Type](entity: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect._

    // check if T is a case-class (e.g. mirrored entity) or a leaf, probably best way to do that
    val quat = QuatMaking.ofType[T]
    quat match
      case _: Quat.Product =>
        '{ ${ liftProduct[T, PrepareRow, Session](entity) }.unquote }
      case _ =>
        var liftPlanter = liftValue[T, PrepareRow, Session](entity)
        '{ $liftPlanter.unquote }
  }

  // TODO Move this method to testing code since this method is only accessed by other macros in the source
  // TODO Use this functionality to test the internals of liftInjectedProduct i.e. see if the correct encoders are summoned
  inline def liftInjectedProductExternal[T, PrepareRow]: List[(String, T => Any)] = ${ liftInjectedProductExternalImpl[T, PrepareRow] }
  def liftInjectedProductExternalImpl[T, PrepareRow](using qctx: Quotes, tpe: Type[T], prepareRowTpe: Type[PrepareRow]): Expr[List[(String, T => Any)]] =
    Expr.ofList {
      liftInjectedProductComponents[T, PrepareRow]._2.map { elem =>
        '{ (${ Expr(elem._1) }, ${ elem._2 }) }
      }
    }

  // TODO Injected => Injectable
  private[getquill] def liftInjectedProduct[T, PrepareRow, Session](using qctx: Quotes, tpe: Type[T], prepareRowTpe: Type[PrepareRow], sessionTpe: Type[Session]): (CaseClass, List[Expr[InjectableEagerPlanter[_, PrepareRow, Session]]]) = {
    import qctx.reflect._
    val (caseClassAstInitial, liftsInitial) = liftInjectedProductComponents[T, PrepareRow]
    val TaggedLiftedCaseClass(caseClassAst, lifts) = TaggedLiftedCaseClass(caseClassAstInitial, liftsInitial).reKeyWithUids()
    val liftPlanters =
      lifts.map((liftKey, lift) =>
        // since we don't have an implicit Type for every single lift, we need to pull out each of their TypeReprs convert them to Type and manually pass them in
        // Also need to widen the type otherwise for some value v=Person(name: String) the type will be TermRef(TermRef(NoPrefix,val v),val name) as oppsoed to 'String'
        val liftType = lift.asTerm.tpe.widen.asType
        liftType match {
          case '[T => liftT] =>
            injectableLiftValue[liftT, PrepareRow, Session](lift.asExprOf[T => liftT], liftKey) // Note: if want to get this to work, try doing 'summon[Type[liftT]]' (using liftType, prepareRowTpe, quotes)
        }
      )
    (caseClassAst, liftPlanters)
  }

  private[getquill] def liftInjectedProductComponents[T, PrepareRow](using qctx: Quotes, tpe: Type[T], prepareRowTpe: Type[PrepareRow]): (CaseClass, List[(String, Expr[T => _])]) = {
    import qctx.reflect._
    import scala.quoted._
    import io.getquill.util.Format

    // Get the elaboration and AST once so that it will not have to be parsed out of the liftedCombo (since they are normally returned by ElaborateStructure.ofProductValue)
    val elaborated = ElaborateStructure.Term.ofProduct[T](ElaborationSide.Encoding)
    val (_, caseClassAst) = ElaborateStructure.productValueToAst[T](elaborated)
    val caseClass = caseClassAst.asInstanceOf[io.getquill.ast.CaseClass]

    // Get just the labels. We need to pass in an instance of T which we don't actually use
    // so it's fine to pass in anything but technically that is a bit inelegant.
    // Unfortunately, we need not only the flattened labels (which would could probably get from the Elaborator)
    // since we need to manually type the lift T=>element-of-T functions e.g. we need to type
    // (p:Person=>p.name) manually as String which we will do below in determining the output.
    // otherwise scala will just think they are 'Any' and we won't be able to summon encoders for them later
    val labels = elaborated.paths

    // Need to parse lifts out of a lambda method and then isolate the clauses later. Need to do it like this
    // instead of just making a fake-variable because doing the latter would violate phase-consistency (i.e. since we would)
    // be using a variable in a phase that does not actually exists. Instead we create a (singleArg) => { List(lift(singleArg.foo), lift(singleArg.bar), etc...)) }
    // ...
    // and the respectively pull out lift(singleArg.foo), lift(singleArg.bar), etc... from that clause turning it into
    // (singleArg) => lift(singleArg.foo), (singleArg) => lift(singleArg.bar), (singleArg) => etc... so that everything remains phase consistent
    val liftLambdasAndTypes = ElaborateStructure.decomposedProductValue[T](ElaborationSide.Encoding) // Elaboration side is 'Encoding' since we are in the lift macro
    val liftLambdas = liftLambdasAndTypes.map(_._1)
    val exprTypes = liftLambdasAndTypes.map(_._2)
    def liftCombo[Output: Type](index: Int) =
      '{ (entity: T) =>
        ${ liftLambdas(index).apply('entity) }.asInstanceOf[Output]
      }

    val output =
      labels.zipWithIndex.map((label, index) => {
        exprTypes(index) match
          case '[tt] => (label, liftCombo[tt](index))
      })

    (caseClass, output)

    // import io.getquill.metaprog.Extractors._
    // val output =
    //   //tmc.UntypeExpr(liftedCombo).asTerm match
    //   liftedCombo.asTerm.underlyingArgument match
    //     case Lambda(params, body) =>
    //       val paramTypes = params.map(_.tpt.tpe)
    //       val paramNames = params.map(_.name)
    //       val mt = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[Any] /*.appliedTo(body.tpe.widen)*/ )
    //       Lambda(Symbol.spliceOwner, mt, (owner,args) => body.changeOwner(owner))
  }

  private[getquill] def liftProduct[T, PrepareRow, Session](productEntity: Expr[T])(using qctx: Quotes, tpe: Type[T], prepareRowTpe: Type[PrepareRow], sessionTpe: Type[Session]): Expr[CaseClassLift[T]] = {
    import qctx.reflect._
    // Elaborate the entity and get it's lift. Since we are in the lifter, the elaboration side is the encoding side (i.e. since lifts are doing Encoding).
    val TaggedLiftedCaseClass(caseClassAst, lifts) = ElaborateStructure.ofProductValue[T](productEntity, ElaborationSide.Encoding).reKeyWithUids()
    val liftPlanters =
      lifts.map((liftKey, lift) =>
        // since we don't have an implicit Type for every single lift, we need to pull out each of their TypeReprs convert them to Type and manually pass them in
        // Also need to widen the type otherwise for some value v=Person(name: String) the type will be TermRef(TermRef(NoPrefix,val v),val name) as oppsoed to 'String'
        val liftType = lift.asTerm.tpe.widen.asType
        liftType match {
          case '[liftT] =>
            liftValue[liftT, PrepareRow, Session](lift.asExprOf[liftT], liftKey) // Note: if want to get this to work, try doing 'summon[Type[liftT]]' (using liftType, prepareRowTpe, quotes)
        }
      )
    val quotation = '{ Quoted[T](${ Lifter(caseClassAst) }, ${ Expr.ofList(liftPlanters) }, Nil) }
    '{ CaseClassLift[T]($quotation, ${ Expr(java.util.UUID.randomUUID.toString) }) } // NOTE UUID technically not needed here. Can try to remove it later
  }

  private[getquill] def summonEncoderOrFail[T: Type, PrepareRow: Type, Session: Type](loggingEntity: Expr[_])(using Quotes) =
    import quotes.reflect._
    Expr.summon[GenericEncoder[T, PrepareRow, Session]] match
      case Some(enc) => enc
      case None      => report.throwError(s"Cannot Find a '${Printer.TypeReprCode.show(TypeRepr.of[T])}' Encoder of ${Printer.TreeShortCode.show(loggingEntity.asTerm)}", loggingEntity)

  private[getquill] def liftValue[T: Type, PrepareRow: Type, Session: Type](valueEntity: Expr[T], uuid: String = newUuid)(using Quotes) /*: Expr[EagerPlanter[T, PrepareRow]]*/ = {
    import quotes.reflect._
    val encoder = summonEncoderOrFail[T, PrepareRow, Session](valueEntity)
    '{ EagerPlanter($valueEntity, $encoder, ${ Expr(uuid) }) } // [T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }

  def valueOrString[T: Type, PrepareRow: Type, Session: Type](valueEntity: Expr[Any], uuid: String = newUuid)(using Quotes) =
    import quotes.reflect._
    // i.e. the actual thing being passed to the encoder e.g. for lift(foo.bar) this will be "foo.bar"
    val fieldName = Format.Expr(valueEntity)
    // The thing being encoded converted to a string, unless it is null then null is returned
    val valueEntityToString = '{ StringOrNull($valueEntity) }
    val nullableEncoder = summonEncoderOrFail[Option[T], PrepareRow, Session](valueEntity)
    val expectedClassTag =
      Expr.summon[ClassTag[T]] match
        case Some(value) => value
        case None        => report.throwError(s"Cannot create a classTag for the type ${Format.TypeOf[T]} for the value ${fieldName}. Cannot create a string-fallback encoder.")
    val converterExpr: Expr[Either[String, FromString[T]]] =
      StringCodec.FromString.summonExpr[T] match
        case Right(value) => '{ Right($value) }
        case Left(msg)    => '{ Left(${ Expr(msg) }) }
    '{
      EagerPlanter[Any, PrepareRow, Session](
        $valueEntity,
        GenericEncoderWithStringFallback($nullableEncoder, $converterExpr)($expectedClassTag),
        ${ Expr(uuid) }
      ).unquote
    }

  private[getquill] def injectableLiftValue[T: Type, PrepareRow: Type, Session: Type](valueEntity: Expr[_ => T], uuid: String = newUuid)(using Quotes) /*: Expr[EagerPlanter[T, PrepareRow]]*/ = {
    import quotes.reflect._
    val encoder =
      Expr.summon[GenericEncoder[T, PrepareRow, Session]] match
        case Some(enc) => enc
        case None => report.throwError(
            s"Cannot inject the value: ${io.getquill.util.Format.Expr(valueEntity)}.Cannot Find a '${Printer.TypeReprCode.show(TypeRepr.of[T])}' Encoder of ${Printer.TreeShortCode.show(valueEntity.asTerm)}",
            valueEntity
          )

    '{ InjectableEagerPlanter($valueEntity, $encoder, ${ Expr(uuid) }) } // [T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }

  def applyLazy[T, PrepareRow](valueEntity: Expr[T])(using Quotes, Type[T], Type[PrepareRow]): Expr[T] = {
    import quotes.reflect._
    val uuid = java.util.UUID.randomUUID().toString
    '{ LazyPlanter($valueEntity, ${ Expr(uuid) }).unquote } // [T, PrepareRow] // adding these causes assertion failed: unresolved symbols: value Context_this
  }
}
