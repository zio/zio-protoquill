package io.getquill

/**
 * These are helper methods for doing various common tasks in cassandra. For example,
 * if you are filtering by non-indexed methods, use the query.allowFiltering method
 * to turn off the cassandra check. Note that this will likely cause a full-table scan.
 *
 * Normally in Quill these are quoted however in ProtoQuill they are not always unquoted e.g.
 * when something like this happens:
 * def peopleByName = quote {
 *  (name: String) => people.filter(p => p.name == name).allowFiltering
 * }
 * since the type Quoted[Q] for allowFiltering is not auto-unquoted, error will happen.
 * This is why these are left as unquoted inline def.
 */
extension [Q <: Query[_]](inline q: Q) {
  inline def allowFiltering = sql"$q ALLOW FILTERING".generic.pure.as[Q]
}

extension [A <: EntityQuery[_]](inline q: A) {
  inline def usingTimestamp(inline ts: Int) = sql"$q USING TIMESTAMP $ts".as[A]
  inline def usingTtl(inline ttl: Int) = sql"$q USING TTL $ttl".as[A]
  inline def using(inline ts: Int, inline ttl: Int) = sql"$q USING TIMESTAMP $ts AND TTL $ttl".as[A]
}

extension [A <: Insert[_]](inline q: A) {
  inline def ifNotExists = sql"$q IF NOT EXISTS".as[A]
  inline def usingTimestamp(inline ts: Int) = sql"$q USING TIMESTAMP $ts".as[A]
  inline def usingTtl(inline ttl: Int) = sql"$q USING TTL $ttl".as[A]
  inline def using(inline ts: Int, inline ttl: Int) = sql"$q USING TIMESTAMP $ts AND TTL $ttl".as[A]
}

extension [A <: Delete[_]](inline q: A) {
  inline def ifExists = sql"$q IF EXISTS".as[A]
  inline def usingTimestamp(inline ts: Int) = sql"$q USING TIMESTAMP $ts".as[A]
  inline def usingTtl(inline ttl: Int) = sql"$q USING TTL $ttl".as[A]
  inline def using(inline ts: Int, inline ttl: Int) = sql"$q USING TIMESTAMP $ts AND TTL $ttl".as[A]
}

extension [T](inline q: Action[T]) {
  inline def ifCond(inline cond: T => Boolean) =
    sql"$q IF $cond".as[Action[T]]
}

extension [K, V](inline map: Map[K, V]) {
  inline def containsValue(inline value: V) = sql"$map CONTAINS $value".as[Boolean]
}
