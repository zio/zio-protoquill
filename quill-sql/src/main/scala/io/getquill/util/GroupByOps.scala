package io.getquill.util

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import _root_.io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import _root_.io.getquill.ast.Visibility.{Hidden, Visible}
import scala.deriving._
import scala.quoted._
import _root_.io.getquill.parser.Lifter
import _root_.io.getquill.quat.Quat

// TODO Move to a utils area
object GroupByOps {
  import collection.immutable.ListSet
  import collection.mutable.{LinkedHashMap, Builder}

  implicit class GroupByOrderedImplicitImpl[A](val t: Traversable[A]) extends AnyVal {
    def groupByOrderedUnique[K](f: A => K): Map[K, ListSet[A]] =
      groupByGen(ListSet.newBuilder[A])(f)

    def groupByOrdered[K](f: A => K): Map[K, List[A]] =
      groupByGen(List.newBuilder[A])(f)

    def groupByGen[K, C[_]](makeBuilder: => Builder[A, C[A]])(f: A => K): Map[K, C[A]] = {
      val map = LinkedHashMap[K, Builder[A, C[A]]]()
      for (i <- t) {
        val key = f(i)
        val builder = map.get(key) match {
          case Some(existing) => existing
          case None =>
            val newBuilder = makeBuilder
            map(key) = newBuilder
            newBuilder
        }
        builder += i
      }
      // Don't need to keep the original map, just map the values in place
      map.mapValues(_.result).toMap // TODO Need to convert this to LinkedHashMap for ordering guarantees?
    }
  }
}
