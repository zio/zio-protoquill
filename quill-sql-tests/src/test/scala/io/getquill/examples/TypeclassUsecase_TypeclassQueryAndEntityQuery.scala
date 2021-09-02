package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object TypeclassUsecase_TypeclassQueryAndEntityQuery {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Node(id: Int, timestamp: Int, status: String)
  case class Master(key: Int, lastCheck: Int, state: String)
  case class Worker(shard: Int, lastTime: Int, reply: String)

  trait GroupKey[T, G]:
    inline def apply(inline t: T): G
  trait EarlierThan[T]:
    inline def apply(inline a: T, inline b: T): Boolean

  inline given GroupKey[Node, Int] with
    inline def apply(inline t: Node): Int = t.id
  inline given GroupKey[Master, Int] with
    inline def apply(inline t: Master): Int = t.key
  inline given GroupKey[Worker, Int] with
    inline def apply(inline t: Worker): Int = t.shard

  inline given EarlierThan[Node] with
    inline def apply(inline a: Node, inline b: Node) = a.timestamp < b.timestamp
  inline given EarlierThan[Master] with
    inline def apply(inline a: Master, inline b: Master) = a.lastCheck < b.lastCheck
  inline given EarlierThan[Worker] with
    inline def apply(inline a: Worker, inline b: Worker) = a.lastTime < b.lastTime

  trait JoiningFunctor[F[_]]:
    extension [A, B](inline xs: F[A])
      inline def map(inline f: A => B): F[B]
      inline def filter(inline f: A => Boolean): F[A]
      inline def leftJoin(inline ys: F[B])(inline f: (A, B) => Boolean): F[(A, Option[B])]

  class QueryJoiningFunctor extends JoiningFunctor[Query]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)
      inline def filter(inline f: A => Boolean): Query[A] = xs.filter(f)
      inline def leftJoin(inline ys: Query[B])(inline f: (A, B) => Boolean): Query[(A, Option[B])] =
        xs.leftJoin(ys).on(f)

  class ListJoiningFunctor extends JoiningFunctor[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)
      inline def filter(inline f: A => Boolean): List[A] = xs.filter(f)
      inline def leftJoin(inline ys: List[B])(inline f: (A, B) => Boolean): List[(A, Option[B])] =
        xs.flatMap { x =>
          val matching = ys.filter(y => f(x, y)).map(y => (x, Some(y)))
          if (matching.length == 0) List((x, None)) else matching
        }

  inline given queryJoiningFunctor: QueryJoiningFunctor = new QueryJoiningFunctor
  inline given listJoiningFunctor: ListJoiningFunctor = new ListJoiningFunctor

  inline given aconversion[T]: Conversion[EntityQuery[T], Query[T]] = (q: EntityQuery[T]) => q: Query[T]
  inline given bconversion[T]: Conversion[Query[T], Query[T]] = (q: Query[T]) => q: Query[T]
  inline given cconversion[T]: Conversion[List[T], List[T]] = (q: List[T]) => q: List[T]

  inline def latestStatus[O[_], F[_], T, G](inline q: O[T])(using
      inline conv: O[T] => F[T],
      inline fun: JoiningFunctor[F],
      inline groupKey: GroupKey[T, G],
      inline earlierThan: EarlierThan[T]
  ): F[T] =
    conv(q).leftJoin(conv(q))((a, b) =>
      groupKey(b) == groupKey(a) &&
        earlierThan(b, a)
    )
      .filter((a, b) =>
        b.map(b => groupKey(b)).isEmpty
      )
      .map((a, b) => a)

  // Need this specialization so you can run latestStatus on a query[T] since that's an EntityQuery
  //inline def latestStatus[T, G](inline q: EntityQuery[T])(using inline fun: JoiningFunctor[Query], inline groupKey: GroupKey[T, G], inline earlierThan: EarlierThan[T]): Query[T] =
  //  latestStatus[Query, T, G](q: Query[T])

  def main(args: Array[String]): Unit = {

    inline def nodesQuery: Query[Node] = quote { latestStatus(query[Node]) }
    inline def mastersQuery: Query[Master] = quote { latestStatus(select[Master]) }
    inline def workersQuery: Query[Worker] = quote { latestStatus(select[Worker]) }

    println(run(nodesQuery).string)
    println(run(mastersQuery).string)
    println(run(workersQuery).string)

    inline def nodesEntityQuery: Query[Node] = quote { latestStatus(query[Node]) }
    inline def mastersEntityQuery: Query[Master] = quote { latestStatus(select[Master]) }
    inline def workersEntityQuery: Query[Worker] = quote { latestStatus(select[Worker]) }

    println(run(nodesEntityQuery).string)
    println(run(mastersEntityQuery).string)
    println(run(workersEntityQuery).string)

    val nodesList = List(Node(1, 1, "UP"), Node(1, 2, "DOWN"), Node(2, 3, "WAITING"))
    println(latestStatus(nodesList))
  }
}
