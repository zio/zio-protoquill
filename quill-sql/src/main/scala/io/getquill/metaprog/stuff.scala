package io.getquill

import scala.annotation.targetName
import io.getquill.quotation.NonQuotedException.apply
import io.getquill.quotation.NonQuotedException

object Forwarders:
  trait Unquoteable[In] {
    type Out
    inline def unquote(inline value: In): Out
  }
  object Unquoteable extends UnquoteableUnwrappableLowPriority {
    trait WithOut[In, Out0] extends Unquoteable[In] { type Out = Out0 }
    inline def apply[In](using inline ev: Unquoteable[In]): Unquoteable[In] = ev
    inline given quotedIsQuoteable[T]: WithOut[Quoted[T], T] with
      type Out = T
      inline def unquote(inline value: Quoted[T]): T = io.getquill.unquote(value)
  }
  trait UnquoteableUnwrappableLowPriority {
    given everyIsQuoteable[T]: Unquoteable.WithOut[T, T] with
      type Out = T
      inline def unquote(inline value: T): T = value
  }

  extension [A, B](inline f: A => B) {
    inline def apply(inline a: Quoted[A]): B = f(io.getquill.unquote(a))
  }

  extension [A, B](inline f: Quoted[A => B]) {
    inline def apply[X](inline a: X)(using inline unquoteable: Unquoteable.WithOut[X, A]): B =
      io.getquill.unquote(f).apply(unquoteable.unquote(a))
  }

// (p: People => p.name)/*Quoted[T] / T*/.apply(person /*Quoted[T], T*/)

// extension [A, B](inline f: A => B)
//   inline def apply1(inline a: Quoted[A]): B = f.apply(io.getquill.unquote(a))

// extension [A, B](inline f: Q[A => B])(using un: Unquoteable[Q])
//   inline def apply(inline a: Quoted[A]) = unquote(f).apply(unquote(a))
// // inline def apply(inline a: A) = unquote(f).apply(a) // Already exists

// extension [Q[_], A](inline oa: Q[Option[A]])(using un: Unquoteable[Q[_]]) {
//   @targetName("isEmptyOption")
//   inline def isEmpty: Boolean = unquote(oa).isEmpty
//   inline def isDefined: Boolean = unquote(oa).isDefined
//   inline def get: A = unquote(oa).get
//   inline def getOrElse[B >: A](inline default: B): B = unquote(oa).getOrElse(default)
//   inline def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = unquote(oa).orNull
//   inline def map[B](inline f: A => B): Option[B] = unquote(oa).map(f)
//   inline def flatMap[B](inline f: A => Option[B]): Option[B] = unquote(oa).flatMap(f)
//   inline def flatten[B](implicit ev: A <:< Option[B]): Option[B] = unquote(oa).flatten
//   inline def filter(inline p: A => Boolean): Option[A] = unquote(oa).filter(p)
//   @targetName("nonEmptyOption")
//   inline def nonEmpty = unquote(oa).nonEmpty
//   inline def exists(inline p: A => Boolean): Boolean = unquote(oa).exists(p)
//   inline def forall(inline p: A => Boolean): Boolean = unquote(oa).forall(p)
//   inline def orElse[B >: A](inline alternative: Option[B]): Option[B] = unquote(oa).orElse(alternative)
// }

// extension [A, B](inline f: A => B) {
//   inline def apply(inline a: Quoted[A]): B = f(io.getquill.unquote(a))
// }

// extension [A, B](inline f: Quoted[A => B]) {
//   inline def apply[X](inline a: X)(using inline unquoteable: Unquoteable.WithOut[X, A]): B =
//     io.getquill.unquote(f).apply(unquoteable.unquote(a))
// }

  extension [T](inline query: Quoted[Query[T]])
    inline def filter[X](f: X)(using inline unquoteable: Unquoteable.WithOut[X, T => Boolean]): Query[T] = io.getquill.unquote(query).filter(unquoteable.unquote(f))

// This seems to work! I don't understand how it works but it doesn't even throw an "methods are same after overloading" exception
//inline def filter[X, R](f: X)(using inline unquoteable: Unquoteable.WithOut[X, T => R]): Query[T] = ??? // io.getquill.unquote(query).filter(unquoteable.unquote(f))

// inline def map[R](inline f: T => R): Query[R] = unquote(query).map(f)
// inline def flatMap[R](inline f: T => Query[R]): Query[R] = unquote(query).flatMap(f)
// inline def distinctOn[R](inline f: T => R): Query[T] = unquote(query).distinctOn(f)

// inline def ++[U >: T](inline q: Query[U]): Query[U] =. unquote(query).++(q)
// inline def unionAll[U >: T](inline q: Query[U]): Query[U] = unquote(query).unionAll(q)
// inline def union[U >: T](inline q: Query[U]): Query[U] = unquote(query).union(q)
// inline def join[A >: T, B](inline q: Query[B]): JoinQuery[A, B, (A, B)] = unquote(query).join(q)
// inline def leftJoin[A >: T, B](inline q: Query[B]): JoinQuery[A, B, (A, Option[B])] = unquote(query).leftJoin(q)
// inline def rightJoin[A >: T, B](inline q: Query[B]): JoinQuery[A, B, (Option[A], B)] = unquote(query).rightJoin(q)
// inline def fullJoin[A >: T, B](inline q: Query[B]): JoinQuery[A, B, (Option[A], Option[B])] = unquote(query).fullJoin(q)

// inline def take(inline n: Int): Query[T] = unquote(query).take(n)
// inline def drop(inline n: Int): Query[T] = unquote(query).drop(n)

// inline def concatMap[R, U](inline f: T => U)(implicit ev: U => Iterable[R]): Query[R] = unquote(query).concatMap(f)
// inline def withFilter(inline f: T => Boolean): Query[T] = unquote(query).withFilter(f)

// inline def sortBy[R](inline f: T => R)(implicit ord: Ord[R]): Query[T] = unquote(query).sortBy(f)
// inline def groupBy[R](inline f: T => R): Query[(R, Query[T])] = unquote(query).groupBy(f)
// inline def groupByMap[G, R](inline by: T => G)(inline mapTo: T => R): Query[R] = unquote(query).groupByMap(by)(mapTo)
// inline def value[U >: T]: Option[T] = unquote(query).value
// inline def min[U >: T]: Option[T] = unquote(query).min
// inline def max[U >: T]: Option[T] = unquote(query).max
// inline def avg[U >: T](implicit inline n: Numeric[U]): Option[BigDecimal] = unquote(query).avg
// inline def sum[U >: T](implicit inline n: Numeric[U]): Option[T] = unquote(query).sum
// inline def size: Long = unquote(query).size
// inline def join[A >: T](inline on: A => Boolean): Query[A] = unquote(query).join(on)
// inline def leftJoin[A >: T](inline on: A => Boolean): Query[Option[A]] = unquote(query).leftJoin(on)
// inline def rightJoin[A >: T](inline on: A => Boolean): Query[Option[A]] = unquote(query).rightJoin(on)
// inline def contains[B >: T](inline value: B): Boolean = unquote(query).contains(value)
// inline def nonEmpty: Boolean = unquote(query).nonEmpty
// inline def isEmpty: Boolean = unquote(query).isEmpty
// inline def distinct: Query[T] = unquote(query).distinct
// inline def nested: Query[T] = unquote(query).nested
// inline def foreach[A <: QAC[_, _] with Action[_], B](inline f: T => B)(implicit unquoteProxy: B => A): BatchAction[A] =
//   unquote(query).foreach(f)
// }

// extension [A, B, R](inline query: Quoted[JoinQuery[A, B, R]]) {
//   inline def on(inline f: (A, B) => Boolean): Query[R] = unquote(query).on(f)
// }

// extension [T](query: Quoted[EntityQuery[T]]) {
//   inline def withFilter(inline f: T => Boolean): EntityQueryModel[T] = unquote(query).withFilter(f)

//   inline def filter(inline f: Quoted[T => Boolean]): EntityQuery[T] = unquote(query).filter(unquote(f))
//   inline def filter(inline f: T => Boolean): EntityQuery[T] = unquote(query).filter(f)

//   inline def map[R](inline f: T => R): EntityQueryModel[R] = unquote(query).map(f)
//   inline def insert(inline f: (T => (Any, Any)), f2: (T => (Any, Any))*): Insert[T] = unquote(query).insert(f)
//   inline def update(inline f: (T => (Any, Any)), f2: (T => (Any, Any))*): Update[T] = unquote(query).update(f)
//   inline def delete: Delete[T] = unquote(query).delete
// }

// extension (query: Insert[E]) {
//   inline def returning[R](inline f: E => R): ActionReturning[E, R] = NonQuotedException()
//   inline def returningGenerated[R](inline f: E => R): ActionReturning[E, R] = NonQuotedException()
//   inline def returningMany[R](inline f: E => R): ActionReturning[E, List[R]] = NonQuotedException()
//   inline def onConflictIgnore: Insert[E] = NonQuotedException()
//   inline def onConflictIgnore(target: E => Any, targets: (E => Any)*): Insert[E] = NonQuotedException()
//   inline def onConflictUpdate(assiginline n: ((E, E) => (Any, Any)), assigns: ((E, E) => (Any, Any))*): Insert[E] = NonQuotedException()
//   inline def onConflictUpdate(target: E => Any, targets: (E => Any)*)(assiginline n: ((E, E) => (Any, Any)), assigns: ((E, E) => (Any, Any))*): Insert[E] = NonQuotedException()
// }

// extension (query: Update[E]) {
//   inline def returning[R](inline f: E => R): ActionReturning[E, R] = NonQuotedException()
//   inline def returningMany[R](inline f: E => R): ActionReturning[E, List[R]] = NonQuotedException()
// }

// extension (query: Delete[E]) {
//   inline def returning[R](inline f: E => R): ActionReturning[E, R] = NonQuotedException()
//   inline def returningMany[R](inline f: E => R): ActionReturning[E, List[R]] = NonQuotedException()
// }
end Forwarders
