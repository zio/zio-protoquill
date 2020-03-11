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

class QuotationTest {
  case class Address(street:String, zip:Int) extends Embedded
  case class Person(name: String, age: Int, address: Address)

  // // test a quotation producing an ast
  @Test
  def compiletime_quotationProducingAst() = {
    inline def q = quote {
      query[Person].map(p => p.name) // also try _.name
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), q.ast)
  }

  @Test
  def compiletime_quotationProducingAstUnquote() = { //helloooooooooooooooooo
    inline def q = quote {
      query[Person] // also try _.name
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
  }

  @Test
  def compiletime_quotationProducingAstUnquote3Level() = { //helloooooooooooooooooo
    inline def q = quote {
      query[Person] // also try _.name
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    // We only need a context to do lifts
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    inline def qqq = quote {
      qq.map(s => s + lift("hello")) // TODO Need to add tests with lift to QueryTest
    }
    printer.lnf(qqq)
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
    println(run(qqq).string)
  }

  // // test a quotation producing an ast
  // // note this is actually fine and should be able to produce a query since
  // // we are not passing one query into another. I.e. there is no QuotationTag
  // // that needs to be joined later
  @Test
  def runtime_oneLevel() = {
    val q = quote {
      query[Person].map(p => p.name) // also try _.name
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), q.ast)
  }

  // // test a quotation going into another
  // // (with/without auto unquoting?)
  @Test
  def runtime_quotationProducingAstAutoUnquote() = { //hello
    val q = quote {
      query[Person]
    }
    val qq = quote {
      q.map(p => p.name)
    }
    val matches = 
      qq match {
        case Quoted(
          Map(QuotationTag(tagId), Ident("p"), Property(Ident("p"), "name")),
          List(),
          List(QuotationVase(Quoted(Entity("Person", List()), List(), List()), vaseId))
        ) if (vaseId == tagId) => true
        case _ => false
      }
    assertTrue(matches)
  }

  @Test
  def compiletime_simpleLift() = {
    import miniquill.context.mirror.Row
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    inline def q = quote {
      lift("hello")
    }
    assertTrue(q match {
      case Quoted(ScalarTag(tagUid), List(ScalarPlanter("hello", encoder, vaseUid)), List()) if (tagUid == vaseUid) => true
      case _ => false
    })
    val vase = 
      q.lifts match {
        case head :: Nil => head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
      }
      
    assertEquals(Row("hello"), vase.encoder.apply(0, vase.value, new Row()))
  }

  @Test
  def runtime_simpleLift() = {
    import miniquill.context.mirror.Row
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    val q = quote {
      lift("hello")
    }
    val qq = quote {
      q
    }
    assertTrue(qq match {
      case 
        Quoted(
          QuotationTag(quotationTagId),
          List(),
          List(
            QuotationVase(
              Quoted(
                ScalarTag(scalarTagId),
                List(ScalarPlanter("hello", _, scalarPlanterId)),
                List()
              ),
              quotationVaseId
            )
          )
        ) if (quotationTagId == quotationVaseId && scalarTagId == scalarPlanterId)
        => true
      case _ => false
    })
    val vase = 
      q.lifts match {
        case head :: Nil => head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
      }
      
    assertEquals(Row("hello"), vase.encoder.apply(0, vase.value, new Row()))
  }

  @Test
  def compiletime_liftPlusOperator() = {
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    inline def q = quote {
      query[Person].map(p => p.name + lift("hello"))
    }
    assertTrue(q match {
      case Quoted(
          Map(Entity("Person", List()), Ident("p"), BinaryOperation(Property(Ident("p"), "name"), StringOperator.+, ScalarTag(tagUid))),
          List(ScalarPlanter("hello", _, planterUid)), // TODO Test what kind of encoder it is? Or try to run it and make sure it works?
          List()
        ) if (tagUid == planterUid) => true
      case _ => false
    })
  }

  @Test
  def compiletime_doubleLiftPlusOperator() = {
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
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
    // TODO Move all expressed value testing to QueryTest
    assertEquals(run(qqq).string, "SELECT p.name || ? FROM Person p")
  }
}

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