package io.getquill

import scala.language.implicitConversions
import io.getquill.quoter.QuotationLot
import io.getquill.quoter.Dsl._
import io.getquill._
import io.getquill.ast._
import io.getquill.quoter.Quoted
import io.getquill.quoter.Planter
import io.getquill.quoter.EagerPlanter
import io.getquill.quoter.LazyPlanter
import io.getquill.quoter.QuotationVase
import io.getquill.quoter.QuotationLot
import io.getquill.dsl.GenericEncoder
import org.scalatest._

class QuotationTest extends Spec with Inside {
  case class Address(street:String, zip:Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)
  import ShortAst._

  "compiletime quotation has correct ast for" - {
    "trivial whole-record select" in {
      inline def q = quote { query[Person] }
      q.ast mustEqual Entity("Person", List())
    }
    "single field mapping" in {
      inline def q = quote { query[Person].map(p => p.name) }
      q.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
    }
    "anonymous single field mapping" in {
      inline def q = quote { query[Person].map(_.name) }
      q.ast mustEqual Map(Entity("Person", List()), Ident("x1"), Property(Ident("x1"), "name"))
    }
    "splice into another quotation without quote" in {
      inline def q = query[Person]
      inline def qq = quote { q.map(p => p.name) }
       qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
    }
    "unquoted splice into another quotation" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
       qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
    }
    "double unquoted splice into another quotation" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      inline def qqq = quote { qq.map(s => s) }
      qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
      qqq.ast mustEqual Map(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), Ident("s"), Ident("s"))
    }
    "double splice into another quotation, middle not quoted" in {
      inline def q = quote { query[Person] }
      inline def qq = q.map(p => p.name)
      inline def qqq = quote { qq.map(s => s) }
       qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
       qqq.ast mustEqual Map(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), Ident("s"), Ident("s"))
    }
    "double unquoted splict with a lift" in {
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))

      val ctx = new MirrorContext(MirrorSqlDialect, Literal) // We only need a context to do lifts
      import ctx._
      inline def qqq = quote { qq.map(s => s + lift("hello")) }
      //println(io.getquill.util.Messages.qprint(qqq.ast))
    }
  }

  "runtime quotation has correct ast for" - {
    "simple one-level query with map" in {
      val q = quote { query[Person].map(p => p.name) }
       q.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))
    }
    "two-level query with map" in {
      val q = quote { query[Person] }
      val qq = quote { q.map(p => p.name) }
      inside(qq) {
        case Quoted(
          Map(QuotationTag(tagId), Ident("p"), Property(Ident("p"), "name")),
          List(),
          List(QuotationVase(Quoted(Entity("Person", List()), List(), List()), vaseId))
        ) if (vaseId == tagId) =>
      }
    }
    "query with a lift" in {
      import io.getquill.context.mirror.Row
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      inline def q = quote {
        lift("hello")
      }
      inside(q) {
        case Quoted(ScalarTag(tagUid), List(EagerPlanter("hello", encoder, vaseUid)), List()) if (tagUid == vaseUid) =>
      }
      val vase = 
        q.lifts match {
          case head :: Nil => head.asInstanceOf[EagerPlanter[String, ctx.PrepareRow /* or just Row */]]
        }
        
      Row("hello") mustEqual vase.encoder.apply(0, vase.value, new Row())
    }
    // TODO Fix here
    "two-level query with a lift" in {
      import io.getquill.context.mirror.Row
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
      val q = quote { lift("hello") }
      val qq = quote { q }
      inside(qq) {
        case 
          Quoted(
            QuotationTag(quotationTagId), 
            Nil,
            List(QuotationVase(Quoted(ScalarTag(scalarTagId), List(EagerPlanter("hello", _, planterId)), Nil), quotationVaseId))
          ) if (quotationTagId == quotationVaseId && scalarTagId == planterId) =>
      }
      val vase = 
        q.lifts match {
          case head :: Nil => head.asInstanceOf[EagerPlanter[String, ctx.PrepareRow /* or just Row */]]
        }
        
      Row("hello") mustEqual vase.encoder.apply(0, vase.value, new Row())
    }
    "query with a lift and plus operator" in {
      val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      import ctx._
  
      inline def q = quote { query[Person].map(p => p.name + lift("hello")) }
      q must matchPattern {
        case Quoted(
            Map(Ent("Person"), Id("p"), Property(Id("p"), "name") `(+)` ScalarTag(tagUid)),
            List(EagerPlanter("hello", encoder, planterUid)), // TODO Test what kind of encoder it is? Or try to run it and make sure it works?
            Nil
          ) if (tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
    }
    "two level query with a lift and plus operator" in {
      case class Address(street:String, zip:Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }
      inline def qq = quote { q.map(p => p.name) }
      qq.ast mustEqual Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name"))

      // We only need a context to do lifts
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._
      inline def qqq = quote { qq.map(s => s + lift("hello")) }
      qqq must matchPattern {
        case Quoted(
            Map(Map(Ent("Person"), Id("p"), Property(Id("p"), "name")), Id("s"), Id("s") `(+)` ScalarTag(tagUid)),
            List(EagerPlanter("hello", encoder, planterUid)), // Compare encoders by ref since all mirror encoders are same case class
            Nil
          ) if (tagUid == planterUid && encoder.eq(summon[Encoder[String]])) =>
      }
    }
    "two level query with a lazy lift, eager lift, lazy lift, and plus operator" in {
      case class Address(street:String, zip:Int) extends Embedded
      case class Person(name: String, age: Int, address: Address)
      inline def q = quote { query[Person] }
      val ctx = new MirrorContext(PostgresDialect, Literal)
      import ctx._
      inline def qq = quote { q.map(p => p.name + lift("hello")) }
      // qq must matchPattern {
      //   case Quoted(
      //     Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name") `(+)` ScalarTag(uid)),
      //     List(LazyPlanter("hello", planterUid)),
      //     Nil
      //   ) if (uid == planterUid) =>
      // }

      // We only need a context to do lifts
      
      inline def qqq = quote { qq.map(s => s + lift("how")) } //hellooooooo
      //println(io.getquill.util.Messages.qprint(qqq))

      
      println("============= PrepareRow ============= " + ctx.run(qqq).prepareRow.data.toList)

      // qqq must matchPattern {
      //   case Quoted(
      //       Map(Map(Ent("Person"), Id("p"), Property(Id("p"), "name") `(+)` ScalarTag(tuid1)), Id("s"), Id("s") `(+)` ScalarTag(tuid2) `(+)` ScalarTag(tuid3)),
      //       List(_), //LazyPlanter("hello", puid1), EagerPlanter("how", encoder, puid2), LazyPlanter("hello", puid3)
      //       Nil
      //     ) /* if (tuid1 == puid1 && tuid2 == puid2 && tuid3 == puid3 && encoder.eq(summon[Encoder[String]])) */ =>
      // }
    }
  }
}


// @main def simpleLift = {
//   import io.getquill.context.mirror.Row

//   case class Person(name: String)

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   inline def q = quote {
//     liftEager("hello")
//   }
//   println(q)
//   val vase = q.lifts.asInstanceOf[Product].productIterator.toList.head.asInstanceOf[Planter[String, ctx.PrepareRow /* or just Row */]]
//   println(vase.encoder.apply(0, vase.value, new Row()))
// }

// @main def runtimeEagerLift = {
//   import io.getquill.context.mirror.Row

//   case class Person(name: String)

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   def q = quote {
//     liftEager("hello")
//   }
//   val qq = quote { q }
//   println(qq)
//   val vase = q.lifts.asInstanceOf[Product].productIterator.toList.head.asInstanceOf[Planter[String, ctx.PrepareRow /* or just Row */]]
//   println(vase.encoder.apply(0, vase.value, new Row()))
// }


// @main def liftAndRun = {
//   import io.getquill.context.mirror.Row

//   case class Person(name: String)

//   inline def q = quote {
//     query[Person].map(p => p.name + lift("foo"))
//   }

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._

//   println(q)

//   println(run(q).string)
//   println(run(q).prepareRow(new Row()))
// }

// @main def identTest = {
//   //import scala.language.implicitConversions

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   val q = quote {
//     lift("hello")
//   }
//   val qq = quote { //hello
//     q
//   }
//   printer.lnf(qq)
//   //val output = run(qq)
// }

// TODO Need to have an error for this scenario!!
// @main def noEncoderTestRuntime = { 
//   //import scala.language.implicitConversions
//   case class Person(name: String, age: Int)

//   class Foo
//   val q = quote {
//     query[Person].map(p => p.name + lift(new Foo))
//   }
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   val qq = quote { //hello
//     q
//   }
//   printer.lnf(qq)
//   val output = run(qq)
// }

// TODO This is a negative test (i.e. encoder finding should not work. Should look into how to write these for dotty)
// @main def noEncoderTestCompile = { 
//   case class Person(name: String, age: Int)

//   class Foo
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   inline def q = quote {
//     query[Person].map(p => p.name + lift(new Foo))
//   }
  
//   inline def qq = quote { //hello
//     q
//   }
//   printer.lnf(qq)
//   val output = run(qq)
// }

// test a runtime quotation
// test two runtime quotations
// test a runtime going into a compiletime


// test a quotation with a context
// test a quotation producing a variable