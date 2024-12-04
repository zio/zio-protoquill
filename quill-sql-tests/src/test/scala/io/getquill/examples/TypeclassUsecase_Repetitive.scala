package io.getquill.examples


import scala.language.implicitConversions
import io.getquill._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import TypeclassExampleEntities.*

object TypeclassUsecase_Repetitive {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  def main(args: Array[String]): Unit = {

    // TODO Add this arrangement as a use-case for the testing of the expander
    inline def nodes = quote {
      (for {
        a <- query[Node]
        b <- query[Node].leftJoin(b =>
          b.id == a.id &&
          b.timestamp > a.timestamp
        )
      } yield (a, b))
      .filter((a, b) => b.map(_.id).isEmpty)
      .map((a, b) => a)
    }

    inline def masters = quote {
      (for {
        a <- query[Master]
        b <- query[Master].leftJoin(b =>
          b.key == a.key &&
          b.lastCheck > a.lastCheck
        )
      } yield (a, b)
      ).filter((a, b) => b.map(_.key).isEmpty)
      .map((a, b) => a)
    }

    inline def workers = quote {
      (for {
        a <- query[Worker]
        b <- query[Worker].leftJoin(b =>
          b.shard == a.shard &&
          b.lastTime > a.lastTime
        )
      } yield (a, b))
      .filter((a, b) => b.map(_.shard).isEmpty)
      .map((a, b) => a)
    }

    println( run(nodes).string )
    println( run(masters).string )
    println( run(workers).string )
  }
}
