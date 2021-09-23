package io.getquill.context.cassandra

import io.getquill.{ Query, Action, Delete, Insert, EntityQuery }
import io.getquill._

object Ops {

  extension [Q <: Query[_]](inline q: Q) {
    inline def allowFiltering = quote(infix"$q ALLOW FILTERING".generic.pure.as[Q])
  }

  extension [A <: EntityQuery[_]](inline q: A) {
    inline def usingTimestamp(ts: Int) = quote(infix"$q USING TIMESTAMP $ts".as[A])
    inline def usingTtl(ttl: Int) = quote(infix"$q USING TTL $ttl".as[A])
    inline def using(ts: Int, ttl: Int) = quote(infix"$q USING TIMESTAMP $ts AND TTL $ttl".as[A])
  }

  extension [A <: Insert[_]](inline q: A) {
    inline def ifNotExists = quote(infix"$q IF NOT EXISTS".as[A])
    inline def usingTimestamp(ts: Int) = quote(infix"$q USING TIMESTAMP $ts".as[A])
    inline def usingTtl(ttl: Int) = quote(infix"$q USING TTL $ttl".as[A])
    inline def using(ts: Int, ttl: Int) = quote(infix"$q USING TIMESTAMP $ts AND TTL $ttl".as[A])
  }

  extension [A <: Delete[_]](inline q: A) {
    inline def ifExists = quote(infix"$q IF EXISTS".as[A])
    inline def usingTimestamp(ts: Int) = quote(infix"$q USING TIMESTAMP $ts".as[A])
    inline def usingTtl(ttl: Int) = quote(infix"$q USING TTL $ttl".as[A])
    inline def using(ts: Int, ttl: Int) = quote(infix"$q USING TIMESTAMP $ts AND TTL $ttl".as[A])
  }

  extension [T](inline q: Action[T]) {
    inline def ifCond(cond: T => Boolean) =
      quote(infix"$q IF $cond".as[Action[T]])
  }

  extension [K, V](inline map: Map[K, V]) {
    inline def containsValue(value: V) = quote(infix"$map CONTAINS $value".as[Boolean])
  }
}
