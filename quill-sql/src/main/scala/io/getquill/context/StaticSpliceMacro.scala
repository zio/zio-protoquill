package io.getquill.context

import scala.quoted._
import io.getquill.ToString
import io.getquill.util.Load
import io.getquill.metaprog.Extractors
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.util.Right
import scala.util.Left
import io.getquill.util.Format
import io.getquill.Quoted
import io.getquill.quat.QuatMaking
import io.getquill.parser.Lifter
import scala.util.Try
import io.getquill.StringCodec
import io.getquill.util.CommonExtensions.Either._
import io.getquill.util.CommonExtensions.Throwable._
import io.getquill.util.CommonExtensions.For._

object StaticSpliceMacro {
  import Extractors._

  private[getquill] object SelectPath:
    def recurseInto(using Quotes)(term: quotes.reflect.Term, accum: List[String] = List()): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        // Recurses through a series of selects do the core identifier e.g:
        // Select(Select(Ident("core"), "foo"), "bar") => recurseInto( {Select(Ident("core"), "foo")}, "bar" +: List("baz") )
        case IgnoreApplyNoargs(Select(inner, pathNode)) => recurseInto(inner, pathNode +: accum)
        case id: Ident                                  => Some((id, accum))
        // If at the core of the nested selects is not a Ident, this does not match
        case other => None

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        // recurse on Module.Something
        case select: Select => recurseInto(select)
        // recurse on Module.SomethingApply() from which the empty-args apply i.e. `()` needs to be ignored
        case select @ IgnoreApplyNoargs(_: Select) => recurseInto(select)
        case id: Ident                             => Some((id, List()))
        case _                                     => None
  end SelectPath

  extension [T](opt: Option[T])
    def nullAsNone =
      opt match
        case Some(null) => None
        case _          => opt

  object DefTerm:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      if (term.tpe.termSymbol.isValDef || term.tpe.termSymbol.isDefDef) Some(term)
      else None

  def isModule(using Quotes)(sym: quotes.reflect.Symbol) =
    import quotes.reflect._
    val f = sym.flags
    f.is(Flags.Module) && !f.is(Flags.Package) && !f.is(Flags.Param) && !f.is(Flags.ParamAccessor) && !f.is(Flags.Method)

  object TermIsModule:
    def unapply(using Quotes)(value: quotes.reflect.Term): Boolean =
      import quotes.reflect.{Try => _, _}
      val tpe = value.tpe.widen
      if (isModule(tpe.typeSymbol))
        true
      else
        false

  /** The term is a static module but not a package */
  object TermOwnerIsModule:
    def unapply(using Quotes)(value: quotes.reflect.Term): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.{Try => _, _}
      Try(value.tpe.termSymbol.owner).toOption.flatMap { owner =>
        val memberType = value.tpe.memberType(owner)
        if (isModule(memberType.typeSymbol))
          Some(memberType)
        else
          None
      }

  def apply[T: Type](valueRaw: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.{Try => _, _}
    import ReflectivePathChainLookup.StringOps._
    import io.getquill.ast._

    val value = valueRaw.asTerm.underlyingArgument

    // TODO summon a Expr[StaticSplicer] using the T type passed originally.
    // Then use LoadModule to get the value of that thing during runtime so we can use it
    // (i.e. see io.getquill.metaprog.SummonParser on how to do that)
    // for primitive types e.g. String, Int, Float etc... rather then making summonable splicers
    // it is easier to just splice them directly, since otherwise those StaticSplicer modules themselves
    // need to be compiled in a previous compilation unit, and we want to define them here.
    // Technically that should be fine because they will only actually be used in the testing code though
    // should think about this more later. For now just do toString to check that stuff from the main return works

    val (pathRoot, selectPath) =
      Untype(value) match
        case SelectPath(pathRoot, selectPath) => (pathRoot, selectPath)
        case other                            =>
          // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
          report.throwError(s"Could not load a static value `${Format.Term(value)}` from ${Printer.TreeStructure.show(other)}")

    val (ownerTpe, path) =
      pathRoot match
        case term @ DefTerm(TermIsModule()) =>
          (pathRoot.tpe, selectPath)
        // TODO Maybe only check module owner if Path is Nil?
        case term @ DefTerm(TermOwnerIsModule(owner)) =>
          (owner, pathRoot.symbol.name +: selectPath)
        case _ =>
          report.throwError(s"Cannot evaluate the static path ${Format.Term(value)}. Neither it's type ${Format.TypeRepr(pathRoot.tpe)} nor the owner of this type is a static module.")

    val module = Load.Module.fromTypeRepr(ownerTpe).toEither.discardLeft(e =>
      // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
      report.throwError(s"Could not look up {${(ownerTpe)}}.${path.mkString(".")} from the object.\nStatic load failed due to: ${e.stackTraceToString}")
    )

    val splicedValue =
      ReflectivePathChainLookup(module, path).discardLeft(msg =>
        report.throwError(s"Could not look up {${(ownerTpe)}}.${path.mkString(".")}. Failed because:\n${msg}")
      )

    val quat = Lifter.quat(QuatMaking.ofType[T])

    def errorMsg(error: String) =
      s"Could not statically splice ${Format.Term(value)} because ${error}"

    val spliceEither =
      for {
        castSplice <- Try(splicedValue.current.asInstanceOf[T]).toEither.mapLeft(e => errorMsg(e.getMessage))
        splicer <- StringCodec.ToSql.summon[T].mapLeft(str => errorMsg(str))
        splice <- Try(splicer.toSql(castSplice)).toEither.mapLeft(e => errorMsg(e.getMessage))
      } yield splice

    val spliceStr =
      spliceEither match
        case Left(msg)    => report.throwError(msg, valueRaw)
        case Right(value) => value

    UnquoteMacro('{ Quoted[T](Infix(List(${ Expr(spliceStr) }), List(), true, false, $quat), Nil, Nil) })
}
