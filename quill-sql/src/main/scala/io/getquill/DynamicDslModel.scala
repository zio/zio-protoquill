package io.getquill

import io.getquill.ast.Ast
import io.getquill.ast.Map
import io.getquill.ast.SortBy
import io.getquill.ast.Take
import io.getquill.ast.Drop
import io.getquill.ast.Union
import io.getquill.ast.UnionAll
import io.getquill.ast.FlatMap
import io.getquill.ast.ConcatMap
import io.getquill.ast.Filter
import io.getquill.ast.InnerJoin
import io.getquill.ast.LeftJoin
import io.getquill.ast.RightJoin
import io.getquill.ast.FullJoin
import io.getquill.ast.JoinType
import io.getquill.ast.Distinct
import io.getquill.ast.DistinctOn
import io.getquill.ast.Nested
import io.getquill.ast.Ident
import io.getquill.ast.GroupBy
import io.getquill.ast.AggregationOperator
import io.getquill.ast.Aggregation
import io.getquill.ast.UnaryOperation
import io.getquill.ast.BinaryOperation
import io.getquill.ast.SetOperator
import io.getquill.quat.Quat
import io.getquill.generic.GenericEncoder
import scala.util.DynamicVariable
import io.getquill.ast.Lift
import io.getquill.ast.FlatJoin
import io.getquill.ast.Join
import io.getquill.ast.Property
import io.getquill.{Action => DslAction}
import io.getquill.ast.Returning
import io.getquill.ast.ReturningGenerated
import io.getquill.ast.OnConflict
import io.getquill.util.Messages.fail
import io.getquill.ast.Assignment
import scala.annotation.targetName
import java.util.UUID
import io.getquill.ast.ScalarTag
import io.getquill.ast.External

case class DynamicAlias[T](property: Quoted[T] => Quoted[Any], name: String)

sealed trait DynamicSet[T, U]

case class DynamicSetValue[T, U](
  property: Quoted[T] => Quoted[U],
  value: Quoted[U]
) extends DynamicSet[T, U]
case class DynamicSetEmpty[T, U]() extends DynamicSet[T, U]

object DynamicQuery {
  def apply[T](p: Quoted[Query[T]]) =
    new DynamicQuery[T](p)
}

sealed class DynamicQuery[+T](val q: Quoted[Query[T]]) {

  private def splice[R](ast: Ast, otherLifts: List[Planter[_, _, _]], otherRuntimeQuotes: List[QuotationVase]) =
    Quoted[R](ast, otherLifts ++ q.lifts, otherRuntimeQuotes ++ q.runtimeQuotes)

  protected[this] def transform[U, V, R](
    f: Quoted[U] => Quoted[V],
    t: (Ast, Ident, Ast) => Ast,
    r: (Ast, List[Planter[_, _, _]], List[QuotationVase]) => R = dyn _
  ) =
    withFreshIdent { v =>
      val sp = splice(v, Nil, Nil)
      val q2 = f(sp)
      r(t(q.ast, v, q2.ast), q.lifts ++ q2.lifts, q.runtimeQuotes ++ q2.runtimeQuotes)
    }(Quat.Generic)

  protected[this] def transformOpt[O, R, TT >: T, D <: DynamicQuery[TT]](
    opt: Option[O],
    f: (Quoted[TT], Quoted[O]) => Quoted[R],
    t: (Quoted[TT] => Quoted[R]) => D,
    thiz: D
  )(implicit enc: GenericEncoder[O, _, _]) =
    opt match {
      case Some(o) =>
        val q2 = spliceLift(o, q.lifts, q.runtimeQuotes)
        t(v => f(v, q2))
      case None =>
        thiz
    }

  def map[R](f: Quoted[T] => Quoted[R]): DynamicQuery[R] =
    transform(f, Map(_, _, _))

  def flatMap[R](f: Quoted[T] => Quoted[Query[R]]): DynamicQuery[R] =
    transform(f, FlatMap(_, _, _))

  def filter(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
    transform(f, Filter(_, _, _))

  def withFilter(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
    filter(f)

  def filterOpt[O](opt: Option[O])(
    f: (Quoted[T], Quoted[O]) => Quoted[Boolean]
  )(implicit enc: GenericEncoder[O, _, _]): DynamicQuery[T] =
    transformOpt(opt, f, filter, this)

  def filterIf(
    cond: Boolean
  )(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
    if (cond) filter(f)
    else this

  def concatMap[R, U](
    f: Quoted[T] => Quoted[U]
  )(implicit ev: U => Iterable[R]): DynamicQuery[R] =
    transform(f, ConcatMap(_, _, _))

  def sortBy[R](
    f: Quoted[T] => Quoted[R]
  )(implicit ord: Ord[R]): DynamicQuery[T] =
    transform(f, SortBy(_, _, _, ord.ord))

  def take(n: Quoted[Int]): DynamicQuery[T] =
    dyn(Take(q.ast, n.ast), q.lifts ++ n.lifts, q.runtimeQuotes ++ n.runtimeQuotes)

  def take(n: Int)(implicit enc: GenericEncoder[Int, _, _]): DynamicQuery[T] =
    val uid = UUID.randomUUID().toString
    val out =
      DynamicQuery[T](
        new Quoted[Query[T]](
          Take(q.ast, ScalarTag(uid, External.Source.Parser)),
          EagerPlanter(n, enc, uid) +: q.lifts,
          q.runtimeQuotes
        )
      )
    out

  def takeOpt(opt: Option[Int])(implicit enc: GenericEncoder[Int, _, _]): DynamicQuery[T] =
    opt match {
      case Some(o) => take(o)
      case None    => this
    }

  def drop(n: Quoted[Int]): DynamicQuery[T] =
    dyn(Drop(q.ast, n.ast), q.lifts ++ n.lifts, q.runtimeQuotes ++ n.runtimeQuotes)

  def drop(n: Int)(implicit enc: GenericEncoder[Int, _, _]): DynamicQuery[T] =
    drop(spliceLift(n, q.lifts, q.runtimeQuotes))

  def dropOpt(opt: Option[Int])(implicit enc: GenericEncoder[Int, _, _]): DynamicQuery[T] =
    opt match {
      case Some(o) => drop(o)
      case None    => this
    }

  def ++[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
    dyn(UnionAll(q.ast, q2.ast), q.lifts ++ q2.lifts, q.runtimeQuotes ++ q2.runtimeQuotes)

  def unionAll[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
    dyn(UnionAll(q.ast, q2.ast), q.lifts ++ q2.lifts, q.runtimeQuotes ++ q2.runtimeQuotes)

  def union[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
    dyn(Union(q.ast, q2.ast), q.lifts ++ q2.lifts, q.runtimeQuotes ++ q2.runtimeQuotes)

  def groupBy[R](f: Quoted[T] => Quoted[R]): DynamicQuery[(R, Query[T])] =
    transform(f, GroupBy(_, _, _))

  private def aggregate(op: AggregationOperator) =
    splice(Aggregation(op, q.ast), Nil, Nil)

  def min[U >: T]: Quoted[Option[T]] =
    aggregate(AggregationOperator.min)

  def max[U >: T]: Quoted[Option[T]] =
    aggregate(AggregationOperator.max)

  def avg[U >: T](implicit n: Numeric[U]): Quoted[Option[T]] =
    aggregate(AggregationOperator.avg)

  def sum[U >: T](implicit n: Numeric[U]): Quoted[Option[T]] =
    aggregate(AggregationOperator.sum)

  def size: Quoted[Long] =
    aggregate(AggregationOperator.size)

  def join[A >: T, B](q2: Quoted[Query[B]]): DynamicJoinQuery[A, B, (A, B)] =
    DynamicJoinQuery(InnerJoin, q, q2)

  def leftJoin[A >: T, B](
    q2: Quoted[Query[B]]
  ): DynamicJoinQuery[A, B, (A, Option[B])] =
    DynamicJoinQuery(LeftJoin, q, q2)

  def rightJoin[A >: T, B](
    q2: Quoted[Query[B]]
  ): DynamicJoinQuery[A, B, (Option[A], B)] =
    DynamicJoinQuery(RightJoin, q, q2)

  def fullJoin[A >: T, B](
    q2: Quoted[Query[B]]
  ): DynamicJoinQuery[A, B, (Option[A], Option[B])] =
    DynamicJoinQuery(FullJoin, q, q2)

  private[this] def flatJoin[R](
    tpe: JoinType,
    on: Quoted[T] => Quoted[Boolean]
  ): DynamicQuery[R] =
    withFreshIdent { v =>
      val q2 = on(splice(v, Nil, Nil))
      dyn(FlatJoin(tpe, q.ast, v, q2.ast), q.lifts ++ q2.lifts, q.runtimeQuotes ++ q2.runtimeQuotes)
    }(Quat.Generic)

  def join[A >: T](on: Quoted[A] => Quoted[Boolean]): DynamicQuery[A] =
    flatJoin(InnerJoin, on)

  def leftJoin[A >: T](
    on: Quoted[A] => Quoted[Boolean]
  ): DynamicQuery[Option[A]] =
    flatJoin(LeftJoin, on)

  def rightJoin[A >: T](
    on: Quoted[A] => Quoted[Boolean]
  ): DynamicQuery[Option[A]] =
    flatJoin(RightJoin, on)

  def nonEmpty: Quoted[Boolean] =
    splice(UnaryOperation(SetOperator.nonEmpty, q.ast), Nil, Nil)

  def isEmpty: Quoted[Boolean] =
    splice(UnaryOperation(SetOperator.isEmpty, q.ast), Nil, Nil)

  def contains[B >: T](value: B)(implicit enc: GenericEncoder[B, _, _]): Quoted[Boolean] =
    contains(spliceLift(value, q.lifts, q.runtimeQuotes))

  def contains[B >: T](value: Quoted[B]): Quoted[Boolean] =
    splice(BinaryOperation(q.ast, SetOperator.contains, value.ast), value.lifts, value.runtimeQuotes)

  def distinct: DynamicQuery[T] =
    dyn(Distinct(q.ast), q.lifts, q.runtimeQuotes)

  def distinctOn[R](f: Quoted[T] => Quoted[R]): DynamicQuery[R] =
    transform(f, DistinctOn(_, _, _))

  def nested: DynamicQuery[T] =
    dyn(Nested(q.ast), q.lifts, q.runtimeQuotes)

  override def toString = q.toString
}

case class DynamicJoinQuery[A, B, R](
  tpe: JoinType,
  q1: Quoted[Query[A]],
  q2: Quoted[Query[B]]
) {
  private def splice[R](ast: Ast) =
    Quoted[R](ast, q1.lifts ++ q2.lifts, q1.runtimeQuotes ++ q2.runtimeQuotes)

  def on(f: (Quoted[A], Quoted[B]) => Quoted[Boolean]): DynamicQuery[R] =
    withFreshIdent { iA =>
      withFreshIdent { iB =>
        val q3 = f(splice(iA), splice(iB))
        dyn(
          Join(tpe, q1.ast, q2.ast, iA, iB, q3.ast),
          q1.lifts ++ q2.lifts ++ q3.lifts,
          q1.runtimeQuotes ++ q2.runtimeQuotes ++ q3.runtimeQuotes
        )
      }(q2.ast.quat) // TODO Verify Quat Later
    }(q1.ast.quat)   // TODO Verify Quat Later
}

case class DynamicEntityQuery[T](override val q: Quoted[EntityQuery[T]]) extends DynamicQuery[T](q) {

  private[this] def dyn[R](ast: Ast, lifts: List[Planter[_, _, _]], runtimeQuotes: List[QuotationVase]) =
    DynamicEntityQuery(Quoted[EntityQuery[R]](ast, q.lifts ++ lifts, q.runtimeQuotes ++ runtimeQuotes))

  override def filter(
    f: Quoted[T] => Quoted[Boolean]
  ): DynamicEntityQuery[T] =
    transform(f, Filter(_, _, _), dyn)

  override def withFilter(
    f: Quoted[T] => Quoted[Boolean]
  ): DynamicEntityQuery[T] =
    filter(f)

  override def filterOpt[O](opt: Option[O])(
    f: (Quoted[T], Quoted[O]) => Quoted[Boolean]
  )(implicit enc: GenericEncoder[O, _, _]): DynamicEntityQuery[T] =
    transformOpt(opt, f, filter, this)

  override def map[R](f: Quoted[T] => Quoted[R]): DynamicEntityQuery[R] =
    transform(f, Map(_, _, _), dyn)

  // Implemented as a extension method for EntityQuery coming from Context
  // since only Context can actually lift the value:T
  // def insertValue(value: T): DynamicInsert[T]
  // Same thing with updateValue
  // def updateValue(value: T): DynamicUpdate[T]

  type DynamicAssignment[U] = ((Quoted[T] => Quoted[U]), U)

  private[this] def propsValuesAndQuotes[S](
    l: List[DynamicSet[S, _]]
  ) =
    l.collect { case s: DynamicSetValue[_, _] =>
      val v                = Ident("v", Quat.Generic)
      val spliceQuote      = Quoted(v, q.lifts, q.runtimeQuotes)
      val setPropertyQuote = s.property(spliceQuote)
      val setValueQuote    = s.value
      (setPropertyQuote, setValueQuote, Assignment(v, setPropertyQuote.ast, setValueQuote.ast))
    }

  def insert(l: DynamicSet[T, _]*): DynamicInsert[T] =
    val outputs       = propsValuesAndQuotes(l.toList)
    val assignemnts   = outputs.map(_._3)
    val lifts         = (outputs.map(_._1.lifts).flatten ++ outputs.map(_._2.lifts).flatten).distinct
    val runtimeQuotes = (outputs.map(_._1.runtimeQuotes).flatten ++ outputs.map(_._2.runtimeQuotes).flatten).distinct
    DynamicInsert(
      Quoted[Insert[T]](
        io.getquill.ast.Insert(DynamicEntityQuery.this.q.ast, assignemnts),
        q.lifts ++ lifts,
        q.runtimeQuotes ++ runtimeQuotes
      )
    )

  def update(sets: DynamicSet[T, _]*): DynamicUpdate[T] =
    val outputs       = propsValuesAndQuotes(sets.toList)
    val assignemnts   = outputs.map(_._3)
    val lifts         = (outputs.map(_._1.lifts).flatten ++ outputs.map(_._2.lifts).flatten).distinct
    val runtimeQuotes = (outputs.map(_._1.runtimeQuotes).flatten ++ outputs.map(_._2.runtimeQuotes).flatten).distinct
    DynamicUpdate(
      Quoted[Update[T]](
        io.getquill.ast.Update(DynamicEntityQuery.this.q.ast, assignemnts),
        q.lifts ++ lifts,
        q.runtimeQuotes ++ runtimeQuotes
      )
    )

  def delete: DynamicDelete[T] =
    DynamicDelete(
      Quoted[Delete[T]](io.getquill.ast.Delete(DynamicEntityQuery.this.q.ast), q.lifts, q.runtimeQuotes)
    )
}

object DynamicAction {
  def apply[A <: DslAction[_]](p: Quoted[A]) =
    new DynamicAction[A] {
      override val q = p
    }
}

sealed trait DynamicAction[A <: DslAction[_]] {
  protected[getquill] def q: Quoted[A]

  override def toString = q.toString
}

object DynamicInsert {
  def apply[E](p: Quoted[Insert[E]]) =
    new DynamicInsert[E] {
      override val q = p
    }
}

trait DynamicInsert[E] extends DynamicAction[Insert[E]] {

  def returning[R](f: Quoted[E] => Quoted[R]): DynamicActionReturning[E, R] =
    withFreshIdent { v =>
      val returnQuote = f(Quoted(v, q.lifts, q.runtimeQuotes))
      DynamicActionReturning[E, R](
        Quoted(
          Returning(q.ast, v, returnQuote.ast),
          q.lifts ++ returnQuote.lifts,
          q.runtimeQuotes ++ returnQuote.runtimeQuotes
        )
      )
    }(Quat.Generic)

  def returningGenerated[R](
    f: Quoted[E] => Quoted[R]
  ): DynamicActionReturning[E, R] =
    withFreshIdent { v =>
      val returnQuote = f(Quoted(v, q.lifts, q.runtimeQuotes))
      DynamicActionReturning[E, R](
        Quoted(
          ReturningGenerated(q.ast, v, returnQuote.ast),
          q.lifts ++ returnQuote.lifts,
          q.runtimeQuotes ++ returnQuote.runtimeQuotes
        )
      )
    }(Quat.Generic)

  def onConflictIgnore: DynamicInsert[E] =
    DynamicInsert[E](
      Quoted[Insert[E]](
        OnConflict(
          DynamicInsert.this.q.ast,
          OnConflict.NoTarget,
          OnConflict.Ignore
        ),
        q.lifts,
        q.runtimeQuotes
      )
    )

  def onConflictIgnore(
    targets: (Quoted[E] => Quoted[Any])*
  ): DynamicInsert[E] = {
    val v = Quoted[E](Ident("v", Quat.Generic), q.lifts, q.runtimeQuotes)
    val quotesAndProperties =
      targets.toList.map { f =>
        val targetQuote = f(v)
        targetQuote.ast match {
          case p: Property => (targetQuote, p)
          case p =>
            fail(s"Invalid ignore column: $p")
        }
      }
    val newProperties    = quotesAndProperties.map(_._2)
    val newLifts         = quotesAndProperties.map(_._1.lifts).flatten.distinct
    val newRuntimeQuotes = quotesAndProperties.map(_._1.runtimeQuotes).flatten.distinct
    DynamicInsert[E](
      Quoted[Insert[E]](
        OnConflict(
          DynamicInsert.this.q.ast,
          OnConflict.Properties(newProperties),
          OnConflict.Ignore
        ),
        q.lifts ++ newLifts,
        q.runtimeQuotes ++ newRuntimeQuotes
      )
    )
  }
}

case class DynamicActionReturning[E, Output](
  q: Quoted[ActionReturning[E, Output]]
) extends DynamicAction[ActionReturning[E, Output]]
case class DynamicUpdate[E](q: Quoted[Update[E]]) extends DynamicAction[Update[E]]
case class DynamicDelete[E](q: Quoted[Delete[E]]) extends DynamicAction[Delete[E]]
