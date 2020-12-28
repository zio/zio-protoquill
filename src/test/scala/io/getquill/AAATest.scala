package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.Dsl._
import io.getquill.quoter.QueryDsl._
import io.getquill.quoter.SchemaMeta
import io.getquill.quoter.QueryMeta
import io.getquill.quoter.InsertMeta
import io.getquill.parser.PrintMac

object AAATest {
  import io.getquill._

  case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {
    // // TODO Test to make sure that at runtime, lazy lifts not supported
    // inline def q = quote {
    //   query[Person].filter(p => p.name == lazyLift("Joe"))
    // }
    // PrintMac(q)

    // val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    // import ctx._
    // val r = run(q)
    // println( r.string )
    // println( r.prepareRow.data.toList)

    // inline def q = quote { query[Person] }
    // inline def qq = quote { q.map(p => p.name) }
    // val ctx = new MirrorContext(MirrorSqlDialect, Literal) // We only need a context to do lifts
    // import ctx._
    // inline def qqq = quote { qq.map(s => s + lift("hello")) }
    // println(io.getquill.util.Messages.qprint(qqq.ast))

    case class Address(street:String, zip:Int) extends Embedded //helloooooooooooo
    case class Person(name: String, age: Int, address: Address)
    inline def q = quote { query[Person] }
    val ctx = new MirrorContext(PostgresDialect, Literal)
    import ctx._
    inline def qq = quote { q.map(p => p.name + lift("hello")) }
    PrintMac(qq)
    println("================ First ================")
    println(io.getquill.util.Messages.qprint(qq)) //hellooooooooooooooooooooooooooooooooooo

    // qq must matchPattern {
    //   case Quoted(
    //     Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name") `(+)` ScalarTag(uid)),
    //     List(LazyPlanter("hello", planterUid)),
    //     Nil
    //   ) if (uid == planterUid) =>
    // }

    // We only need a context to do lifts
    
    println("================ Second ================")
    inline def qqq = quote { qq.map(s => s + lift("how")) } //hellooooooooooooooooooooo
    PrintMac(qqq)
    println(io.getquill.util.Messages.qprint(qqq))

    
    println("============= PrepareRow ============= " + ctx.run(qqq).prepareRow.data.toList)
  }
}