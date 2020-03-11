package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.QuotationBin
import miniquill.quoter.QuoteDsl._
import io.getquill._
import io.getquill.ast._
import org.junit.Test
import org.junit.Assert._
import miniquill.quoter.Quoted
import miniquill.quoter.ScalarPlanter
import miniquill.quoter.QuotationVase
import miniquill.quoter.QuotationBin
import io.getquill.context.ExecutionType._

case class Address(street:String, zip:Int) extends Embedded
case class Person(name: String, age: Int, address: Address)

@main def liftAndQuery = { //helloooooooooooooooooo
  case class Address(street:String, zip:Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)

  inline def q = quote {
    query[Person] // also try _.name
  }
  inline def qq = quote {
    q.map(p => p.name)
  }
  // We only need a context to do lifts
  val ctx = new MirrorContext(PostgresDialect, Literal) //hello
  import ctx._
  inline def qqq = quote {
    qq.map(s => s + lift("hello"))
  }
  printer.lnf(qqq)

  printer.lnf(run(qqq))

  // Should not be a dynamic context, this is because in Context this:
  // "ScalarPlanterExpr.InlineList(liftsExpr) => liftsExpr" messes up.
  // That's because "case ScalarPlanterExpr.Inline(vaseExpr) => vaseExpr"
  // does not match properly once the types are re-cast into ScalarPlanter[Any, Any]
  assertEquals(run(qqq).executionType, Static)

  assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
  // TODO Move all expressed value testing to QueryTest
  assertEquals(run(qqq).string, "SELECT p.name || ? FROM Person p")
  
}

// @main def simpleLift = {
//   import miniquill.context.mirror.Row

//   case class Person(name: String)

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   inline def q = quote {
//     liftEager("hello")
//   }
//   println(q)
//   val vase = q.lifts.asInstanceOf[Product].productIterator.toList.head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
//   println(vase.encoder.apply(0, vase.value, new Row()))
// }

// @main def runtimeEagerLift = {
//   import miniquill.context.mirror.Row

//   case class Person(name: String)

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   def q = quote {
//     liftEager("hello")
//   }
//   val qq = quote { q }
//   println(qq)
//   val vase = q.lifts.asInstanceOf[Product].productIterator.toList.head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
//   println(vase.encoder.apply(0, vase.value, new Row()))
// }


// @main def liftAndRun = {
//   import miniquill.context.mirror.Row

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
