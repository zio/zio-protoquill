package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object InlineMacroTest1MostRecentStatusTypeclassesDual {
  import io.getquill._
  case class Address(street: String, zip: Int) extends Embedded
  given Embedable[Address]
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Node(id: Int, timestamp: Int, status: String)
  case class Master(key: Int, lastCheck: Int, state: String)
  case class Worker(shard: Int, lastTime: Int, reply: String)


  trait GroupKey[T, G]:
    inline def apply(inline t: T): G
  trait EarlierThan[T]:
    inline def apply(inline a: T, inline b: T): Boolean
  
  class NodeGroupKey extends GroupKey[Node, Int]:
    inline def apply(inline t: Node): Int = t.id
  class MasterGroupKey extends GroupKey[Master, Int]:
    inline def apply(inline t: Master): Int = t.key
  class WorkerGroupKey extends GroupKey[Worker, Int]: 
    inline def apply(inline t: Worker): Int = t.shard

  inline given nodeGroupKey as NodeGroupKey = new NodeGroupKey
  inline given workerGroupKey as WorkerGroupKey = new WorkerGroupKey
  inline given masterGroupKey as MasterGroupKey = new MasterGroupKey

  class NodeEarlierThan extends EarlierThan[Node]: 
    inline def apply(inline a: Node, inline b: Node) = a.timestamp < b.timestamp
  class MasterEarlierThan extends EarlierThan[Master]: 
    inline def apply(inline a: Master, inline b: Master) = a.lastCheck < b.lastCheck
  class WorkerEarlierThan extends EarlierThan[Worker]: 
    inline def apply(inline a: Worker, inline b: Worker) = a.lastTime < b.lastTime

  inline given nodeEarlierThan as NodeEarlierThan = new NodeEarlierThan
  inline given workerEarlierThan as WorkerEarlierThan = new WorkerEarlierThan
  inline given masterEarlierThan as MasterEarlierThan = new MasterEarlierThan

  trait JoiningFunctor[F[_]]:
    extension [A, B](inline xs: F[A])
      inline def map(inline f: A => B): F[B]
      inline def filter(inline f: A => Boolean): F[A]
      inline def leftJoin(inline ys: F[B])(inline f: (A, B) => Boolean): F[(A, Option[B])]

  class ListJoiningFunctor extends JoiningFunctor[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)
      inline def filter(inline f: A => Boolean): List[A] = xs.filter(f)
      inline def leftJoin(inline ys: List[B])(inline f: (A, B) => Boolean): List[(A, Option[B])] =
        xs.flatMap { x =>
          val matching = ys.filter(y => f(x, y)).map(y => (x, Some(y)))
          if (matching.length == 0) List((x, None)) else matching
        }

  inline given listJoiningFunctor as ListJoiningFunctor = new ListJoiningFunctor
  
  type QueryType[T] = Query[T] // EntityQuery[T] | EntityQueryModel[T] | 

  class EntityQueryJoiningFunctor extends JoiningFunctor[EntityQuery]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)
      inline def filter(inline f: A => Boolean): Query[A] = xs.filter(f)
      inline def leftJoin(inline ys: Query[B])(inline f: (A, B) => Boolean): Query[(A, Option[B])] = 
        xs.leftJoin(ys).on(f)

  class QueryJoiningFunctor extends JoiningFunctor[QueryType]:
    extension [A, B](inline xs: QueryType[A])
      inline def map(inline f: A => B): QueryType[B] = xs.map(f)
      inline def filter(inline f: A => Boolean): QueryType[A] = xs.filter(f)
      inline def leftJoin(inline ys: QueryType[B])(inline f: (A, B) => Boolean): QueryType[(A, Option[B])] = 
        xs.leftJoin(ys).on(f)

  // class QueryJoiningFunctor extends QJoiningFunctor[Query]
  // class EntityQueryJoiningFunctor extends QJoiningFunctor[EntityQuery]


  inline given queryJoiningFunctor as QueryJoiningFunctor = new QueryJoiningFunctor
  inline given entityQueryJoiningFunctor as EntityQueryJoiningFunctor = new EntityQueryJoiningFunctor

  def main(args: Array[String]): Unit = {
      
    inline def latestStatus[F[_], T, G](inline q: F[T])(using inline fun: JoiningFunctor[F], inline groupKey: GroupKey[T, G], inline earlierThan: EarlierThan[T]): F[T] =
      q.leftJoin(q)((a, b) => 
          groupKey(b) == groupKey(a) &&
          earlierThan(b, a)
      )
      .filter((a, b) => 
        b.map(b => groupKey(b)).isEmpty)
      .map((a, b) => a)

    inline def nodes: EntityQuery[Node] = query[Node]
    inline def masters: Query[Master] = query[Master]
    inline def workers: Query[Worker] = query[Worker]

    inline def nodesLatest = quote { latestStatus(nodes) }
    inline def mastersLatest = quote { latestStatus(masters) }
    inline def workersLatest = quote { latestStatus(workers) }

    println( run(nodesLatest).string )
    println( run(mastersLatest).string )
    println( run(workersLatest).string )

    val nodesList = List(Node(1, 1, "UP"), Node(1, 2, "DOWN"), Node(2, 3, "WAITING"))
    println( latestStatus(nodesList) )


    // inline def doBlah[T](inline q: Query[T] | EntityQuery[T])(inline f: T => Boolean) =
    //   q.filter(f)

    // case class Person(name: String, age:Int)
    // inline def qq = quote { doBlah(query[Person])(p => p.name == "Joe") }
    // println( run(qq) )



    // println( run(nodes).string )
    // println( run(masters).string )
    // println( run(workers).string )
  }
}
// hellooooooooooooo