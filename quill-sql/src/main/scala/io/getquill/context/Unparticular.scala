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
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.ast.{Query => AQuery, _}
import scala.util.{Success, Failure}
import io.getquill.idiom.Statement
import io.getquill.QAC
import io.getquill.NamingStrategy

object Unparticular:
  import io.getquill.ast._
  import io.getquill.util.Interleave
  import io.getquill.idiom.StatementInterpolator._
  import scala.annotation.tailrec
  import io.getquill.idiom._

  /**
   * Query with potentially non enumerate liftQuery(...) statements where set operations
   * that look like this: `query[Person].filter(p => liftQuery(scalars).contains(p.name))`
   * will look like this "WHERE p.name in (?)" (this is the basicQuery).
   * This last "?" actually needs to be expanded
   * into a comma separated list coming from the lifted list which is actually Expr[List[T]]
   * but that will be done in the Particularize(r). The `realQuery` is a tokenized representation
   * of the query that can be turned into what it actually will need to look like by the
   * Particularize(r)
   */
  case class Query(basicQuery: String, realQuery: Statement)
  object Query:
    def fromStatement(stmt: Statement, liftingPlaceholder: Int => String) =
      val (basicQuery, lifts) = token2string(stmt, liftingPlaceholder)
      (Query(basicQuery, stmt), lifts)

  def translateNaive(stmt: Statement, liftingPlaceholder: Int => String): String =
    token2string(stmt, liftingPlaceholder)._1

  private def token2string(token: Token, liftingPlaceholder: Int => String): (String, List[External]) = {
    @tailrec
    def apply(
        workList: List[Token],
        sqlResult: Seq[String],
        liftingResult: Seq[External],
        liftingSize: Int
    ): (String, List[External]) = workList match {
      case Nil => sqlResult.reverse.mkString("") -> liftingResult.reverse.toList
      case head :: tail =>
        head match {
          case StringToken(s2)            => apply(tail, s2 +: sqlResult, liftingResult, liftingSize)
          case SetContainsToken(a, op, b) => apply(stmt"$a $op ($b)" +: tail, sqlResult, liftingResult, liftingSize)
          case ScalarTagToken(tag)        => apply(tail, liftingPlaceholder(liftingSize) +: sqlResult, tag +: liftingResult, liftingSize + 1)
          case Statement(tokens)          => apply(tokens.foldRight(tail)(_ +: _), sqlResult, liftingResult, liftingSize)
          case ValuesClauseToken(stmt)    => apply(stmt +: tail, sqlResult, liftingResult, liftingSize)
          case _: ScalarLiftToken =>
            throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
          case _: QuotationTagToken =>
            throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
        }
    }

    apply(List(token), Seq(), Seq(), 0)
  }

end Unparticular
