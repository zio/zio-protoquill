package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object TypeclassUsecase_Encapsulated {
  import io.getquill._
  case class Address(street: String, zip: Int) extends Embedded
  given Embedable[Address]
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Node(id: Int, timestamp: Int, status: String)
  case class Master(key: Int, lastCheck: Int, state: String)
  case class Worker(shard: Int, lastTime: Int, reply: String)

  

  def main(args: Array[String]): Unit = {

    // TODO Add this arrangement as a use-case for the testing of the expander
    // inline def nodes = quote {
    //   (for {
    //     a <- query[Node]
    //     b <- query[Node].leftJoin(b => 
    //       b.id == a.id && 
    //       b.timestamp > a.timestamp
    //     )
    //   } yield (a, b))
    //   .filter((a, b) => b.map(_.id).isEmpty)
    //   .map((a, b) => a)
    // }

    // inline def masters = quote {
    //   (for {
    //     a <- query[Master]
    //     b <- query[Master].leftJoin(b => 
    //       b.key == a.key && 
    //       b.lastCheck > a.lastCheck
    //     )
    //   } yield (a, b)
    //   ).filter((a, b) => b.map(_.key).isEmpty)
    //   .map((a, b) => a)
    // }

    // inline def workers = quote {
    //   (for {
    //     a <- query[Worker]
    //     b <- query[Worker].leftJoin(b => 
    //       b.shard == a.shard && 
    //       b.lastTime > a.lastTime
    //     )
    //   } yield (a, b))
    //   .filter((a, b) => b.map(_.shard).isEmpty)
    //   .map((a, b) => a)
    // }

    
    
    // Should add this to tests since it tested ident parser .name property
    // (i.e. since we passed 'n' to it it Ident(value) => value returned 'n' nead of b)
    // which we had to get by doing id @ Ident and then id.symbol.name
    inline def latestStatus[T, G](
      inline q: Query[T])( 
      inline groupKey: T => G, 
      inline earlierThan: (T, T) => Boolean
    ) =
      q.leftJoin(q)
      .on((a, b) => 
          groupKey(b) == groupKey(a) &&
          earlierThan(b, a)
      )
      .filter((a, b) => 
        b.map(b => groupKey(b)).isEmpty)
      .map((a, b) => a)

    inline def nodesLatest = quote {
      latestStatus(query[Node])(
        n => n.id, 
        (a, b) => a.timestamp < b.timestamp)
    }
    inline def mastersLatest = quote {
      latestStatus(query[Master])(
        m => m.key, 
        (a, b) => a.lastCheck < b.lastCheck)
    }
    inline def workersLatest = quote {
      latestStatus(query[Worker])(
        w => w.shard, 
        (a, b) => a.lastTime < b.lastTime)
    }
    println( run(nodesLatest).string )
    println( run(mastersLatest).string )
    println( run(workersLatest).string )


    // println( run(nodes).string )
    // println( run(masters).string )
    // println( run(workers).string )
  }
}
