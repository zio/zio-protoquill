package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import io.getquill.dsl.EncodingDsl
import io.getquill.quoter.Quoted
import io.getquill.quoter.QueryMeta
import io.getquill.derived._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.dsl.GenericDecoder
import io.getquill.quoter.Planter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{ Transform, QuotationTag }
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.QuotedExpr
import io.getquill.quoter.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.ast.{ Query => AQuery, _ }

import io.getquill._

object StaticTranslationMacro {
  import io.getquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import io.getquill.quoter.Planter
  import io.getquill.idiom.LoadNaming
  import io.getquill.util.LoadObject
  import io.getquill.dsl.GenericEncoder
  import io.getquill.ast.External

  // Process the AST during compile-time. Return `None` if that can't be done.
  private[getquill] def processAst[T: Type](astExpr: Expr[Ast], idiom: Idiom, naming: NamingStrategy)(using Quotes):Option[(String, List[External])] = {
    import io.getquill.ast.{CollectAst, QuotationTag}

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty

    // val queryMeta = 
    //   Expr.summon[QueryMeta]

    val unliftedAst = Unlifter.apply(astExpr)

    if (noRuntimeQuotations(unliftedAst)) {
      
    val expandedAst = unliftedAst match
      case _: AQuery => Elaborate.static[T](unliftedAst)
      case _ => unliftedAst

      //println("Expanded Ast Is: " + expandedAst)

      //println("=========== Unlifted Ast =======\n" + io.getquill.util.Messages.qprint(expandedAst))
      val (ast, stmt) = idiom.translate(expandedAst)(using naming)
      val output =
        ReifyStatement(
          idiom.liftingPlaceholder,
          idiom.emptySetContainsToken,
          stmt,
          forProbing = false
        )
      Some(output)
    } else {
      None
    }
  }

  // Process compile-time lifts, return `None` if that can't be done.
  // liftExprs = Lifts that were put into planters during the quotation. They are
  // 're-planted' back into the PreparedStatement vars here.
  // matchingExternals = the matching placeholders (i.e 'lift tags') in the AST 
  // that contains the UUIDs of lifted elements. We check against list to make
  // sure that that only needed lifts are used and in the right order.
  private[getquill] def processLifts(liftExprs: Expr[List[Planter[_, _]]], matchingExternals: List[External])(using Quotes): Option[List[Expr[Planter[_, _]]]] = {
    import quotes.reflect.report

    val extractedEncodeables =
      liftExprs match {
        case PlanterExpr.UprootableList(lifts) =>
          // get all existing expressions that can be encoded
          Some(lifts.map(e => (e.uid, e)).toMap)
        case _ => 
          // TODO Maybe do ctx.error here to show the lifts to the user, if 'verbose mode' is enabled.
          // Try it out to see how it looks
          println("Lifts could not be extracted during compile-time: '" + liftExprs.show + "' are they 'inline def'?"); 
          None
      }

    extractedEncodeables.map { encodeables => 
      matchingExternals.collect {
        case tag: ScalarTag =>
          encodeables.get(tag.uid) match {
            case Some(encodeable) => encodeable
            case None =>
              report.throwError(s"Invalid Transformations Encountered. Cannot find lift with ID: ${tag.uid}.")
              // TODO Throw an error here or attempt to resolve encoders during runtime?
              // maybe the user has hand-modified a quoted block and only the
              // lifts are modified with some runtime values?
              // If throwing an error (or maybe even not?) need to tell the user which lifted ids cannot be found
              // should probably add some info to the reifier to "highlight" the question marks that
              // cannot be plugged in. It would also be really nice to show the user which lift-statements
              // are wrong but that requires a bit more thought (maybe match them somehow into the original AST
              // from quotedRow via the UUID???)
          }
      }.map(_.plant) // todo dedupe here?
    }
  }

  def idiomAndNamingStatic[D <: Idiom, N <: NamingStrategy](using Quotes, Type[D], Type[N]): Try[(Idiom, NamingStrategy)] =
    for {
      idiom <- LoadObject[D]
      namingStrategy <- LoadNaming.static[N]
    } yield (idiom, namingStrategy)

  
  // def apply[T: Type, D <: Idiom, N <: NamingStrategy](
  //   quotedRaw: Expr[Quoted[Query[T]]]
  // )(using qctx:Quotes, dialectTpe:Type[D], namingType:Type[N]): Expr[Option[( String, List[Planter[_, _]] )]] = {
  //   applyInner(quotedRaw) match {
  //     case Some((query, lifts)) => '{ Some(${Expr(query)}, ${lifts}) }
  //     case None => '{ None }
  //   }
  // }

  def applyInner[Q[_]: Type, T: Type, D <: Idiom, N <: NamingStrategy](
    quotedRaw: Expr[Quoted[Q[T]]]
  )(using qctx:Quotes, dialectTpe:Type[D], namingType:Type[N]): Option[StaticState] = 
  {
    import quotes.reflect.{Try => TTry, _}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr

    import scala.util.{Success, Failure}
    idiomAndNamingStatic match {
      case Success(v) =>
      case Failure(f) => f.printStackTrace()
    }

    extension [T](opt: Option[T]) {
      def errPrint(str: String) = 
        opt match {
          case s: Some[T] => s
          case None => println(str); None
        }
    }

    val tryStatic =
      for {
        (idiom, naming)          <- idiomAndNamingStatic.toOption.errPrint("Could not parse Idiom/Naming")
        // TODO (MAJOR) Really should plug quotedExpr into here because inlines are spliced back in but they are not properly recognized by QuotedExpr.uprootableOpt for some reason
        quotedExpr               <- QuotedExpr.uprootableOpt(quoted).errPrint("Could not uproot the quote") 
        (queryString, externals) <- processAst[T](quotedExpr.ast, idiom, naming).errPrint("Could not process the ASt")
        encodedLifts             <- processLifts(quotedExpr.lifts, externals).errPrint("Could not process the lifts")
      } yield {
        println(
          "Compile Time Query Is: " + 
            (if (System.getProperty("quill.macro.log.pretty", "false") == "true") idiom.format(queryString)
            else queryString)
        )

        // What about a missing decoder?
        // need to make sure that that kind of error happens during compile time
        // (also need to propagate the line number, talk to Li Houyi about that)
        StaticState(queryString, encodedLifts)
      }

    if (tryStatic.isEmpty)
      println("WARNING: Dynamic Query Detected: ")

    tryStatic
  }
}
