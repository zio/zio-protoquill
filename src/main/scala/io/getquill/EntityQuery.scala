package io.getquill

import io.getquill.quotation.NonQuotedException
import io.getquill.context.InsertMacro

object EntityQuery {
  def apply[T] = new EntityQuery[T]() { }
}

trait EntityQuery[T] extends EntityQueryModel[T] {

  // Copied comment from EntityQueryModel. Keep around for information purposes
  // Note: This class is to be shared with Dotty and the parameter `value` needs to be inline.
  //       however, regular values cannot be overridden with inline ones so we cannot define
  //       insert[T] and update[T] on this level.
  //def insert(value: T): Insert[T] = NonQuotedException()
  //def update(value: T): Update[T] = NonQuotedException()

  // Members declared in io.getquill.EntityQueryModel
  // def delete: io.getquill.Delete[T] = NonQuotedException()
  // override def filter(f: T => Boolean): io.getquill.EntityQueryModel[T] = NonQuotedException()
  // def insert(f: T => (Any, Any), f2: Seq[T => (Any, Any)]): io.getquill.Insert[T] = NonQuotedException()
  // override def map: [R](f: T => R): io.getquill.EntityQueryModel[R] = NonQuotedException()
  // def update(f: T => (Any, Any), f2: Seq[T => (Any, Any)]): io.getquill.Update[T] = NonQuotedException()
  // override def withFilter(f: T => Boolean): io.getquill.EntityQueryModel[T] = NonQuotedException()
  
  //override def delete: Delete[T] = NonQuotedException()

  // Variant of this where value is inline? insert
  
  //def insert(f: (T => (Any, Any)), f2: (T => (Any, Any))*): Insert[T] = NonQuotedException()

  
  //def update(f: (T => (Any, Any)), f2: (T => (Any, Any))*): Update[T] = NonQuotedException()
  
  //def map[R](f: T => R): EntityQuery[R] = NonQuotedException()

  // def flatMap[R](f: T => Query[R]): EntityQuery[R] = NonQuotedException()

  // def concatMap[R, U](f: T => U)(implicit ev: U => Iterable[R]): Query[R] = NonQuotedException()

  // def withFilter(f: T => Boolean): EntityQuery[T] = NonQuotedException()
  // def filter(f: T => Boolean): EntityQuery[T] = NonQuotedException()

  // def sortBy[R](f: T => R)(implicit ord: Ord[R]): Query[T] = NonQuotedException()

  // def take(n: Int): Query[T] = NonQuotedException()
  // def drop(n: Int): Query[T] = NonQuotedException()

  // def ++[U >: T](q: Query[U]): Query[U] = NonQuotedException()
  // def unionAll[U >: T](q: Query[U]): Query[U] = NonQuotedException()
  // def union[U >: T](q: Query[U]): Query[U] = NonQuotedException()

  // def groupBy[R](f: T => R): Query[(R, Query[T])] = NonQuotedException()

  // def value[U >: T]: Option[T] = NonQuotedException()
  // def min[U >: T]: Option[T] = NonQuotedException()
  // def max[U >: T]: Option[T] = NonQuotedException()
  // def avg[U >: T](implicit n: Numeric[U]): Option[BigDecimal] = NonQuotedException()
  // def sum[U >: T](implicit n: Numeric[U]): Option[T] = NonQuotedException()
  // def size: Long = NonQuotedException()

  // def join[A >: T, B](q: Query[B]): JoinQuery[A, B, (A, B)] = NonQuotedException()
  // def leftJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (A, Option[B])] = NonQuotedException()
  // def rightJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (Option[A], B)] = NonQuotedException()
  // def fullJoin[A >: T, B](q: Query[B]): JoinQuery[A, B, (Option[A], Option[B])] = NonQuotedException()

  // def join[A >: T](on: A => Boolean): Query[A] = NonQuotedException()
  // def leftJoin[A >: T](on: A => Boolean): Query[Option[A]] = NonQuotedException()
  // def rightJoin[A >: T](on: A => Boolean): Query[Option[A]] = NonQuotedException()

  // def nonEmpty: Boolean = NonQuotedException()
  // def isEmpty: Boolean = NonQuotedException()
  // def contains[B >: T](value: B): Boolean = NonQuotedException()

  // def distinct: Query[T] = NonQuotedException()

  // def nested: Query[T] = NonQuotedException()

  // def foreach[A <: Action[_], B](f: T => B)(implicit unquote: B => A): BatchAction[A] = NonQuotedException()
}