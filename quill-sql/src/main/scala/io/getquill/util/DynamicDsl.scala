package io.getquill

import io.getquill.ast.Renameable.Fixed

import scala.language.implicitConversions
import io.getquill.ast._
import io.getquill.quat._
import io.getquill.util.Messages._

import scala.util.DynamicVariable
import scala.reflect.ClassTag
import io.getquill.{ActionReturning, Delete, EntityQuery, Insert, Ord, Query, Quoted, Update, Action => DslAction}

import scala.annotation.tailrec
import java.util.UUID
import io.getquill.generic.GenericEncoder

private object DynamicDsl {
  private[this] val nextIdentId = new DynamicVariable(0)

  private[getquill] def withFreshIdent[R](f: Ident => R)(quat: Quat): R = {
    val idx = nextIdentId.value
    nextIdentId.withValue(idx + 1) {
      f(Ident(s"v$idx", quat))
    }
  }

  private[getquill] def dyn[T](ast: Ast): DynamicQuery[T] =
    DynamicQueryPlain[T](splice[Query[T]](ast))

  private[getquill] def splice[T](a: Ast) =
    Quoted[T](a, Nil, Nil)

  private[getquill] def spliceLift[O](o: O)(implicit enc: GenericEncoder[O, _, _]) =
    val uid = UUID.randomUUID.toString
    Quoted[O](ScalarTag(uid), List(EagerPlanter(o, enc, uid)), Nil)
}

case class DynamicQueryPlain[+T](protected[getquill] val q: Quoted[Query[T]]) extends DynamicQuery[T]

extension [T](q: Quoted[Query[T]])
  def dynamic: DynamicQuery[T] = DynamicQueryPlain(q)

sealed trait DynamicQuery[+T] {
  import DynamicDsl._

  protected[getquill] def q: Quoted[Query[T]]

  protected[this] def transform[U, V, R](
      f: Quoted[U] => Quoted[V],
      t: (Ast, Ident, Ast) => Ast,
      r: Ast => R = dyn _
  ) =
    withFreshIdent { v =>
      r(t(q.ast, v, f(splice(v)).ast))
    }(Quat.Generic)

  // protected[this] def transformOpt[O, R, D <: DynamicQuery[T]](
  //     opt: Option[O],
  //     f: (Quoted[T], Quoted[O]) => Quoted[R],
  //     t: (Quoted[T] => Quoted[R]) => D,
  //     thiz: D
  // )(implicit enc: Encoder[O]) =
  //   opt match {
  //     case Some(o) =>
  //       t(v => f(v, spliceLift(o)))
  //     case None =>
  //       thiz
  //   }

  def map[R](f: Quoted[T] => Quoted[R]): DynamicQuery[R] =
    transform(f, Map.apply)

  // def flatMap[R](f: Quoted[T] => Quoted[Query[R]]): DynamicQuery[R] =
  //   transform(f, FlatMap)

  // def filter(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
  //   transform(f, Filter)

  // def withFilter(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
  //   filter(f)

  // def filterOpt[O](opt: Option[O])(
  //     f: (Quoted[T], Quoted[O]) => Quoted[Boolean]
  // )(implicit enc: Encoder[O]): DynamicQuery[T] =
  //   transformOpt(opt, f, filter, this)

  // def filterIf(
  //     cond: Boolean
  // )(f: Quoted[T] => Quoted[Boolean]): DynamicQuery[T] =
  //   if (cond) filter(f)
  //   else this

  // def concatMap[R, U](
  //     f: Quoted[T] => Quoted[U]
  // )(implicit ev: U => Iterable[R]): DynamicQuery[R] =
  //   transform(f, ConcatMap)

  // def sortBy[R](
  //     f: Quoted[T] => Quoted[R]
  // )(implicit ord: Ord[R]): DynamicQuery[T] =
  //   transform(f, SortBy(_, _, _, ord.ord))

  def take(n: Quoted[Int]): DynamicQuery[T] =
    dyn(Take(q.ast, n.ast))

  def take(n: Int)(implicit enc: GenericEncoder[Int, _, _]): DynamicQuery[T] =
    take(spliceLift(n))

  // def takeOpt(opt: Option[Int]): DynamicQuery[T] =
  //   opt match {
  //     case Some(o) => take(o)
  //     case None    => this
  //   }

  // def drop(n: Quoted[Int]): DynamicQuery[T] =
  //   dyn(Drop(q.ast, n.ast))

  // def drop(n: Int): DynamicQuery[T] =
  //   drop(spliceLift(n))

  // def dropOpt(opt: Option[Int]): DynamicQuery[T] =
  //   opt match {
  //     case Some(o) => drop(o)
  //     case None    => this
  //   }

  // def ++[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
  //   dyn(UnionAll(q.ast, q2.ast))

  // def unionAll[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
  //   dyn(UnionAll(q.ast, q2.ast))

  // def union[U >: T](q2: Quoted[Query[U]]): DynamicQuery[U] =
  //   dyn(Union(q.ast, q2.ast))

  // def groupBy[R](f: Quoted[T] => Quoted[R]): DynamicQuery[(R, Query[T])] =
  //   transform(f, GroupBy)

  // private def aggregate(op: AggregationOperator) =
  //   splice(Aggregation(op, q.ast))

  // def min[U >: T]: Quoted[Option[T]] =
  //   aggregate(AggregationOperator.min)

  // def max[U >: T]: Quoted[Option[T]] =
  //   aggregate(AggregationOperator.max)

  // def avg[U >: T](implicit n: Numeric[U]): Quoted[Option[T]] =
  //   aggregate(AggregationOperator.avg)

  // def sum[U >: T](implicit n: Numeric[U]): Quoted[Option[T]] =
  //   aggregate(AggregationOperator.sum)

  // def size: Quoted[Long] =
  //   aggregate(AggregationOperator.size)

  // def join[A >: T, B](q2: Quoted[Query[B]]): DynamicJoinQuery[A, B, (A, B)] =
  //   DynamicJoinQuery(InnerJoin, q, q2)

  // def leftJoin[A >: T, B](
  //     q2: Quoted[Query[B]]
  // ): DynamicJoinQuery[A, B, (A, Option[B])] =
  //   DynamicJoinQuery(LeftJoin, q, q2)

  // def rightJoin[A >: T, B](
  //     q2: Quoted[Query[B]]
  // ): DynamicJoinQuery[A, B, (Option[A], B)] =
  //   DynamicJoinQuery(RightJoin, q, q2)

  // def fullJoin[A >: T, B](
  //     q2: Quoted[Query[B]]
  // ): DynamicJoinQuery[A, B, (Option[A], Option[B])] =
  //   DynamicJoinQuery(FullJoin, q, q2)

  // private[this] def flatJoin[R](
  //     tpe: JoinType,
  //     on: Quoted[T] => Quoted[Boolean]
  // ): DynamicQuery[R] =
  //   withFreshIdent { v =>
  //     dyn(FlatJoin(tpe, q.ast, v, on(splice(v)).ast))
  //   }(Quat.Generic)

  // def join[A >: T](on: Quoted[A] => Quoted[Boolean]): DynamicQuery[A] =
  //   flatJoin(InnerJoin, on)

  // def leftJoin[A >: T](
  //     on: Quoted[A] => Quoted[Boolean]
  // ): DynamicQuery[Option[A]] =
  //   flatJoin(LeftJoin, on)

  // def rightJoin[A >: T](
  //     on: Quoted[A] => Quoted[Boolean]
  // ): DynamicQuery[Option[A]] =
  //   flatJoin(RightJoin, on)

  // def nonEmpty: Quoted[Boolean] =
  //   splice(UnaryOperation(SetOperator.nonEmpty, q.ast))

  // def isEmpty: Quoted[Boolean] =
  //   splice(UnaryOperation(SetOperator.isEmpty, q.ast))

  // def contains[B >: T](value: B)(implicit enc: Encoder[B]): Quoted[Boolean] =
  //   contains(spliceLift(value))

  // def contains[B >: T](value: Quoted[B]): Quoted[Boolean] =
  //   splice(BinaryOperation(q.ast, SetOperator.contains, value.ast))

  // def distinct: DynamicQuery[T] =
  //   dyn(Distinct(q.ast))

  // def distinctOn[R](f: Quoted[T] => Quoted[R]): DynamicQuery[R] =
  //   transform(f, DistinctOn)

  // def nested: DynamicQuery[T] =
  //   dyn(Nested(q.ast))

  override def toString = q.toString
}

case class DynamicEntityQuery[T](q: Quoted[EntityQuery[T]])
    extends DynamicQuery[T] {

  import DynamicDsl._

  private[this] def dyn[R](ast: Ast) =
    DynamicEntityQuery(splice[EntityQuery[R]](ast))

  // override def filter(
  //     f: Quoted[T] => Quoted[Boolean]
  // ): DynamicEntityQuery[T] =
  //   transform(f, Filter, dyn)

  // override def withFilter(
  //     f: Quoted[T] => Quoted[Boolean]
  // ): DynamicEntityQuery[T] =
  //   filter(f)

  // override def filterOpt[O](opt: Option[O])(
  //     f: (Quoted[T], Quoted[O]) => Quoted[Boolean]
  // )(implicit enc: Encoder[O]): DynamicEntityQuery[T] =
  //   transformOpt(opt, f, filter, this)

  override def map[R](f: Quoted[T] => Quoted[R]): DynamicEntityQuery[R] =
    transform(f, Map.apply, dyn)

  // def insertValue(value: T): DynamicInsert[T] = macro DynamicQueryDslMacro.insertValue

  // type DynamicAssignment[U] = ((Quoted[T] => Quoted[U]), U)

  // private[this] def assignemnts[S](
  //     l: List[DynamicSet[S, _]]
  // ): List[Assignment] =
  //   l.collect {
  //     case s: DynamicSetValue[_, _] =>
  //       val v = Ident("v", Quat.Generic)
  //       Assignment(v, s.property(splice(v)).ast, s.value.ast)
  //   }

  // def insert(l: DynamicSet[T, _]*): DynamicInsert[T] =
  //   DynamicInsert(
  //     splice(Insert(DynamicEntityQuery.this.q.ast, assignemnts(l.toList)))
  //   )

  // def updateValue(value: T): DynamicUpdate[T] = macro DynamicQueryDslMacro.updateValue

  // def update(sets: DynamicSet[T, _]*): DynamicUpdate[T] =
  //   DynamicUpdate(
  //     splice[Update[T]](
  //       Update(DynamicEntityQuery.this.q.ast, assignemnts(sets.toList))
  //     )
  //   )

  // def delete: DynamicDelete[T] =
  //   DynamicDelete(splice[Delete[T]](Delete(DynamicEntityQuery.this.q.ast)))
}
