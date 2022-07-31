package io.getquill.context

import io.getquill.EagerPlanter
import io.getquill.InjectableEagerPlanter
import io.getquill.EagerListPlanter
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.LazyPlanterExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.LazyPlanter
import io.getquill.Planter
import io.getquill.ast.Ast
import io.getquill.ast.{Map => AMap, _}
import io.getquill.util.Interleave
import io.getquill.idiom.StatementInterpolator._
import scala.annotation.tailrec
import io.getquill.idiom._
import scala.quoted._
import io.getquill.util.Format
import io.getquill.metaprog.InjectableEagerPlanterExpr
import io.getquill.parser.Lifter

/**
 * For a query that has a filter(p => liftQuery(List("Joe","Jack")).contains(p.name)) we need to turn
 * the "WHERE p.name in (?)" into WHERE p.name in (?, ?) i.e. to "Particularize" the query
 * to the number of elements in the query lift. In Scala2-Quill we could just access the values
 * of the liftQuery list directly since the lift was an 'Any' value directly in the AST.
 * In Scala 3 however, we need to treat the lifted list as an Expr and create an Expr[String]
 * that represents the Query that is to be during runtime based on the content of the list
 * which has to be manipulated inside of a '{ ... } block.
 */
object Particularize:
  private[getquill] object UnparticularQueryLiftable:
    def apply(token: Unparticular.Query)(using Quotes) = liftableUnparticularQuery(token)
    extension [T](t: T)(using ToExpr[T], Quotes) def expr: Expr[T] = Expr(t)
    import io.getquill.parser.BasicLiftable

    given liftableUnparticularQuery: BasicLiftable[Unparticular.Query] with
      def lift =
        case Unparticular.Query(basicQuery: String, realQuery: Statement) =>
          '{ Unparticular.Query(${ basicQuery.expr }, ${ StatementLiftable(realQuery) }) }
  end UnparticularQueryLiftable

  private[getquill] object StatementLiftable:
    def apply(token: Statement)(using Quotes) = liftableStatement(token)
    extension [T](t: T)(using ToExpr[T], Quotes) def expr: Expr[T] = Expr(t)
    import io.getquill.parser.BasicLiftable

    given liftableToken: BasicLiftable[Token] with
      def lift =
        // Note strange errors about SerializeHelper.fromSerialized types can happen here if NotSerializing is not true.
        // Anyway we do not want tag-serialization here for the sake of simplicity for the tokenization which happens at runtime.
        // AST serialization is generally used to make unlifting deeply nested ASTs simpler but Quotation/Scalar Tags are only 1-level deep.
        case ScalarTagToken(lift: ScalarTag)       => '{ io.getquill.idiom.ScalarTagToken(${ Lifter.NotSerializing.scalarTag(lift) }) }
        case QuotationTagToken(lift: QuotationTag) => '{ io.getquill.idiom.QuotationTagToken(${ Lifter.NotSerializing.quotationTag(lift) }) }
        case StringToken(string)                   => '{ io.getquill.idiom.StringToken(${ string.expr }) }
        case s: Statement                          => liftableStatement(s)
        case SetContainsToken(a, op, b)            => '{ io.getquill.idiom.SetContainsToken(${ a.expr }, ${ op.expr }, ${ b.expr }) }
        case ScalarLiftToken(lift)                 => quotes.reflect.report.throwError("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")

    given liftableStatement: BasicLiftable[Statement] with
      def lift =
        case Statement(tokens) => '{ io.getquill.idiom.Statement(${ tokens.expr }) }
  end StatementLiftable

  // the following should test for that: update - extra lift + scalars + liftQuery/setContains
  object Static:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp: Type](query: Unparticular.Query, lifts: List[Expr[Planter[_, _, _]]], runtimeLiftingPlaceholder: Expr[Int => String], emptySetContainsToken: Expr[Token => Token])(using Quotes): Expr[String] =
      import quotes.reflect._
      val liftsExpr: Expr[List[Planter[?, ?, ?]]] = Expr.ofList(lifts)
      val queryExpr: Expr[Unparticular.Query] = UnparticularQueryLiftable(query)
      '{ Dynamic[PrepareRowTemp]($queryExpr, $liftsExpr, $runtimeLiftingPlaceholder, $emptySetContainsToken) }
  end Static

  object Dynamic:
    /** Convenience constructor for doing particularization from an Unparticular.Query */
    def apply[PrepareRowTemp](
        query: Unparticular.Query,
        lifts: List[Planter[_, _, _]],
        liftingPlaceholder: Int => String,
        emptySetContainsToken: Token => Token
    ): String =
      raw(query.realQuery, lifts, liftingPlaceholder, emptySetContainsToken)

    private[getquill] def raw[PrepareRowTemp, Session](statements: Statement, lifts: List[Planter[_, _, _]], liftingPlaceholder: Int => String, emptySetContainsToken: Token => Token): String = {
      enum LiftChoice:
        case ListLift(value: EagerListPlanter[Any, PrepareRowTemp, Session])
        case SingleLift(value: Planter[Any, PrepareRowTemp, Session])

      val listLifts = lifts.collect { case e: EagerListPlanter[_, _, _] => e.asInstanceOf[EagerListPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val singleLifts = lifts.collect { case e: EagerPlanter[_, _, _] => e.asInstanceOf[EagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap
      val injectableLifts = lifts.collect { case e: InjectableEagerPlanter[_, _, _] => e.asInstanceOf[InjectableEagerPlanter[Any, PrepareRowTemp, Session]] }.map(lift => (lift.uid, lift)).toMap

      def getLifts(uid: String): LiftChoice =
        listLifts.get(uid).map(LiftChoice.ListLift(_))
          .orElse(singleLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .orElse(injectableLifts.get(uid).map(LiftChoice.SingleLift(_)))
          .getOrElse {
            throw new IllegalArgumentException(s"Cannot find list-lift with UID ${uid} (from all the lifts ${lifts})")
          }

      // TODO Also need to account for empty tokens but since we actually have a reference to the list can do that directly
      def placeholders(uid: String, initialIndex: Int): (Int, String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) =>
            // using index 1 since SQL prepares start with $1 typically
            val liftsPlaceholder =
              lifts.values.zipWithIndex.map((_, index) => liftingPlaceholder(index + initialIndex)).mkString(", ")
            val liftsLength = lifts.values.length
            (liftsLength, liftsPlaceholder)
          case LiftChoice.SingleLift(lift) =>
            (1, liftingPlaceholder(initialIndex))

      def isEmptyListLift(uid: String) =
        getLifts(uid) match
          case LiftChoice.ListLift(lifts) => lifts.values.isEmpty
          case _                          => false

      def token2String(token: Token): String = {
        @tailrec
        def apply(
            workList: List[Token],
            sqlResult: Seq[String],
            placeholderIndex: Int // I.e. the index of the '?' that is inserted in the query (that represents a lift)
        ): String = workList match {
          case Nil => sqlResult.reverse.foldLeft("")((concatonation, nextExpr) => concatonation + nextExpr)
          case head :: tail =>
            head match {
              case StringToken(s2) => apply(tail, s2 +: sqlResult, placeholderIndex)
              case SetContainsToken(a, op, b) =>
                b match
                  case ScalarTagToken(tag) if isEmptyListLift(tag.uid) =>
                    apply(emptySetContainsToken(a) +: tail, sqlResult, placeholderIndex)
                  case _ =>
                    apply(stmt"$a $op ($b)" +: tail, sqlResult, placeholderIndex)
              case ScalarTagToken(tag) =>
                val (liftsLength, lifts) = placeholders(tag.uid, placeholderIndex)
                apply(tail, lifts +: sqlResult, placeholderIndex + liftsLength)
              case Statement(tokens) => apply(tokens.foldRight(tail)(_ +: _), sqlResult, placeholderIndex)
              case _: ScalarLiftToken =>
                throw new UnsupportedOperationException("Scalar Lift Tokens are not used in Dotty Quill. Only Scalar Lift Tokens.")
              case _: QuotationTagToken =>
                throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
            }
        }
        apply(List(token), Seq(), 0)
      }

      token2String(statements)
    }
  end Dynamic

end Particularize
