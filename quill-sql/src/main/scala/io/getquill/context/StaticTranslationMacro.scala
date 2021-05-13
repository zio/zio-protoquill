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
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{ Transform, QuotationTag }
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.ast.{ Query => AQuery, _ }
import scala.util.{Success, Failure}
import io.getquill.idiom.Statement
import io.getquill.QAC
import io.getquill.NamingStrategy
import io.getquill.context.QueryExecution.ElaborationBehavior

object StaticTranslationMacro {
  import io.getquill.parser._
  import scala.quoted._ // Expr.summon is actually from here
  import io.getquill.Planter
  import io.getquill.idiom.LoadNaming
  import io.getquill.util.LoadObject
  import io.getquill.generic.GenericEncoder
  import io.getquill.ast.External
  import io.getquill.ReturnAction
  import io.getquill.NamingStrategy

  // Process the AST during compile-time. Return `None` if that can't be done.
  private[getquill] def processAst[T: Type](astExpr: Expr[Ast], elaborate: ElaborationBehavior, idiom: Idiom, naming: NamingStrategy)(using Quotes):Option[(Unparticular.Query, List[External], Option[ReturnAction], Ast)] =
    import io.getquill.ast.{CollectAst, QuotationTag}

    def noRuntimeQuotations(ast: Ast) =
      CollectAst.byType[QuotationTag](ast).isEmpty

    // val queryMeta = 
    //   Expr.summon[QueryMeta]

    val unliftedAst = Unlifter.apply(astExpr)

    if (noRuntimeQuotations(unliftedAst)) {      
      val expandedAst = elaborate match
        // if the AST is a Query, e.g. Query(Entity[Person], ...) we expand it out until something like
        // Map(Query(Entity[Person], ...), x, CaseClass(name: x.name, age: x.age)). This was based on the Scala2-Quill
        // flatten method in ValueProjection.scala. Technically this can be performed in the SqlQuery from the Quat info
        // but the old mechanism is still used because the Quat information might not be there.
        case ElaborationBehavior.Elaborate => ElaborateStructure.ontoAst[T](unliftedAst)
        case ElaborationBehavior.Skip => unliftedAst

      val (ast, stmt) = idiom.translate(expandedAst)(using naming)
      
      val liftColumns =
        (ast: Ast, stmt: Statement) => Unparticular.translateNaive(stmt, idiom.liftingPlaceholder)

      val returningAction = expandedAst match
        // If we have a returning action, we need to compute some additional information about how to return things.
        // Different database dialects handle these things differently. Some allow specifying a list of column-names to
        // return from the query. Others compute this information from the query data directly. This information is stored
        // in the dialect and therefore is computed here.
        case r: ReturningAction =>
          Some(io.getquill.norm.ExpandReturning.applyMap(r)(liftColumns)(idiom, naming))
        case _ =>
          None

      val (unparticularQuery, externals) = Unparticular.Query.fromStatement(stmt, idiom.liftingPlaceholder)
      Some((unparticularQuery, externals, returningAction, expandedAst))
    } else {
      None
    }
  end processAst

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

  def applyInner[I: Type, T: Type, D <: Idiom, N <: NamingStrategy](
    quotedRaw: Expr[Quoted[QAC[I, T]]],
    elaborate: ElaborationBehavior
  )(using qctx:Quotes, dialectTpe:Type[D], namingType:Type[N]): Option[StaticState] = 
  {
    import quotes.reflect.{Try => TTry, _}
    // NOTE Can disable if needed and make quoted = quotedRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val quoted = quotedRaw.asTerm.underlyingArgument.asExpr
    idiomAndNamingStatic[D, N] match {
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
        (idiom, naming)                        <- idiomAndNamingStatic.toOption.errPrint("Could not parse Idiom/Naming")
        // TODO (MAJOR) Really should plug quotedExpr into here because inlines are spliced back in but they are not properly recognized by QuotedExpr.uprootableOpt for some reason
        quotedExpr                             <- QuotedExpr.uprootableOpt(quoted).errPrint("Could not uproot the quote") 
        (query, externals, returnAction, ast)  <- processAst[T](quotedExpr.ast, elaborate, idiom, naming).errPrint("Could not process the ASt")
        encodedLifts                           <- processLifts(quotedExpr.lifts, externals).errPrint("Could not process the lifts")
      } yield {
        if (io.getquill.util.Messages.debugEnabled)
          report.info(
            "Compile Time Query Is: " + 
              (
                if (io.getquill.util.Messages.prettyPrint) 
                  idiom.format(query.basicQuery)
                else 
                  query.basicQuery
              )
          )
        StaticState(query, encodedLifts, returnAction)(ast)
      }

    if (tryStatic.isEmpty)
      println("WARNING: Dynamic Query Detected: ")

    tryStatic
  }
}
