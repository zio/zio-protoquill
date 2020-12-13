package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.Dsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.QueryMeta
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder
import miniquill.quoter.ScalarPlanter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.idiom.{ Idiom => Id }
  import io.getquill.{ NamingStrategy => Na }

import io.getquill._

object RunDynamicTest {

  def apply[RawT, T, D <: Id, N <: Na](ast: Ast): String = {
    ast.toString
  }
}

object RunDslRet {

    import miniquill.parser.TastyMatchers

  trait SummonHelper[ResultRow: Type] {
    implicit val qctx: Quotes
    import qctx.reflect._

    /** Summon decoder for a given Type and Row type (ResultRow) */
    def summonDecoderOrThrow[DecoderT: Type]: Expr[GenericDecoder[ResultRow, DecoderT]] = {
      Expr.summon[GenericDecoder[ResultRow, DecoderT]] match {
        case Some(decoder) => decoder
        case None => report.throwError("Decoder could not be summoned")
      }
    }
  }

  trait QueryMetaHelper[T: Type] extends TastyMatchers {
    // See if there there is a QueryMeta mapping T to some other type RawT
    def summonMetaIfExists =
      Expr.summon[QueryMeta[T, _]] match {
        case Some(expr) =>
          // println("Summoned! " + expr.show)
          UntypeExpr(expr) match {
            case '{ QueryMeta.apply[k, n]($one, $two, $uid) } => Some('[n])
            case _ => report.throwError("Summoned Query Meta But Could Not Get Type")
          }
        case None => None
      }
  }


  class RunTest[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[T]],
    ctx: Expr[Context[D, N]]
  )(using val qctx: Quotes) extends SummonHelper[ResultRow] with QueryMetaHelper[T] with TastyMatchers {
    import qctx.reflect._

    def apply(): Expr[String] = executeQueryDynamic(quoted)

    def executeQueryDynamic(quotedBlock: Expr[Quoted[T]]): Expr[String] = {
      val quotedAst = '{ $quoted.ast }
      '{  RunDynamicTest.apply[T, T, D, N]($quotedAst).asInstanceOf[String] }
    }
  }



  def runTestImpl[T: Type, ResultRow: Type, PrepareRow: Type, D <: Id: Type, N <: Na: Type, Res: Type](
    quoted: Expr[Quoted[T]],
    ctx: Expr[Context[D, N]]
  )(using qctx: Quotes): Expr[String] = {
    new RunTest[T, ResultRow, PrepareRow, D, N, Res](quoted, ctx).apply()
  }
}