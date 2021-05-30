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
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.util.Format

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

  /**
   * There are some cases where we actually do not want to use all of the lifts in a Quoted.
   * For example:
   * {{ query[Person].insert(_.id -> lift(1), _.name -> lift("Joe")).returningGenerated(_.id)) }}
   * becomes something like:
   * {{ Quoted(query[Person].insert(_.id -> lift(A), _.name -> lift(B)).returningGenerated(_.id)), lifts: List(ScalarTag(A, 1), ScalarTag(B, "Joe"))) }}
   * but since we are excluding the person.id column (this is done in the transformation phase NormalizeReturning which is in SqlNormalization in the quill-sql-portable module)
   * actually we only want only the ScalarTag(B) so we need to get the list of lift tags (in tokens) once the Dialect has serialized the query
   * which correctly order the list of lifts. A similar issue happens with insertMeta and updateMeta.
   * Process compile-time lifts, return `None` if that can't be done.
   * liftExprs = Lifts that were put into planters during the quotation. They are
   * 're-planted' back into the PreparedStatement vars here.
   * matchingExternals = the matching placeholders (i.e 'lift tags') in the AST
   * that contains the UUIDs of lifted elements. We check against list to make
   * sure that that only needed lifts are used and in the right order.
   */
  private[getquill] def processLifts(lifts: List[PlanterExpr[_, _]], matchingExternals: List[External])(using Quotes): Option[List[PlanterExpr[_, _]]] =
    import quotes.reflect.report

    val encodeablesMap =
      lifts.map(e => (e.uid, e)).toMap

    val uidsOfScalarTags =
      matchingExternals.collect {
        case tag: ScalarTag => tag.uid
      }

    val sortedEncodeables =
      uidsOfScalarTags.map { uid =>
        encodeablesMap.get(uid) match
          case Some(encodeable) => encodeable
          case None =>
            report.throwError(s"Invalid Transformations Encountered. Cannot find lift with ID: ${uid}.")
      }

    // TODO This should be logged if some fine-grained debug logging is enabled. Maybe as part of some phase that can be enabled via -Dquill.trace.types config
    // val remaining = encodeables.removedAll(uidsOfScalarTags)
    // if (!remaining.isEmpty)
    //   println(s"Ignoring the following lifts: [${remaining.map((_, v) => Format.Expr(v.plant)).mkString(", ")}]")
    Some(sortedEncodeables)
  end processLifts

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
          case None =>
            // TODO Print this when a high level of trace debugging is enabled
            //println(str);
            None
        }
    }

    val tryStatic =
      for {
        (idiom, naming)                        <- idiomAndNamingStatic.toOption.errPrint("Could not parse Idiom/Naming")
        // TODO (MAJOR) Really should plug quotedExpr into here because inlines are spliced back in but they are not properly recognized by QuotedExpr.uprootableOpt for some reason
        (quotedExpr, lifts)                    <- QuotedExpr.uprootableWithLiftsOpt(quoted).errPrint("Could not uproot the quote")
        (query, externals, returnAction, ast)  <- processAst[T](quotedExpr.ast, elaborate, idiom, naming).errPrint("Could not process the ASt")
        encodedLifts                           <- processLifts(lifts, externals).errPrint("Could not process the lifts")
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
        StaticState(query, encodedLifts, returnAction, idiom)(ast)
      }

    if (tryStatic.isEmpty)
      // TODO Only if a high trace level is enabled
      //def additionalMessage = Format.Expr(quotedRaw)
      def additionalMessage = ""
      report.info(s"Dynamic Query Detected${additionalMessage}")

    tryStatic
  }
}
