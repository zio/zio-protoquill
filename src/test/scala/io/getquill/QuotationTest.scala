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
  def compiletime_quotationProducingAstUnquote() = {
    inline def q = quote {
      query[Person] // also try _.name
    }
    inline def qq = quote {
      q.map(p => p.name)
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), qq.ast)
  }

  // test a quotation producing an ast
  // note this is actually fine and should be able to produce a query since
  // we are not passing one query into another. I.e. there is no QuotationTag
  // that needs to be joined later
  @Test
  def runtime_quotationProducingAst() = {
    val q = quote {
      query[Person].map(p => p.name) // also try _.name
    }
    assertEquals(Map(Entity("Person", List()), Ident("p"), Property(Ident("p"), "name")), q.ast)
  }

  // test a quotation going into another
  // (with/without auto unquoting?)
  @Test
  def runtime_quotationProducingAstAutoUnquote() = { //hello
    val q = quote {
      query[Person]
    }
    val qq = quote {
      q.map(p => p.name)
    }
    printer.lnf(qq)
    val matches = 
      qq match {
        case Quoted(
          Map(QuotationTag(_), Ident("p"), Property(Ident("p"), "name")), 
          (QuotationBin(Quoted(Entity("Person", List()), ()), _) *: ())
        ) => true
        case _ => false
      }
    assertTrue(matches)
  }


  @Test
  def compiletime_simpleLift() = {
    inline def q = quote {
      lift("hello")
    }
    assertTrue(q match {
      case Quoted(ScalarTag(tagUid), (ScalarPlanter("hello", vaseUid) *: ())) if (tagUid == vaseUid) => true
      case _ => false
    })
  }

  @Test
  def compiletime_simpleLift_eager() = {
    import miniquill.context.mirror.Row
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    inline def q = quote {
      lift("hello")
    }
    assertTrue(q match {
      case Quoted(ScalarTag(tagUid), (ScalarPlanter("hello", encoder, vaseUid) *: ())) if (tagUid == vaseUid) => true
      case _ => false
    })
    val vase = 
      q.lifts.asInstanceOf[Product].productIterator.toList match {
        case head :: Nil => head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
      }
      
    assertEquals(Row("hello"), vase.encoder.apply(0, vase.value, new Row()))
  }

  // @Test
  // def compiletime_simpleLift_runtime() = {
  //   import miniquill.context.mirror.Row
  //   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  //   import ctx._
  //   def q = quote {
  //     lift("hello")
  //   }
  //   def qq = quote {
  //     q
  //   }
  //   printer.lnf(qq)
  //   assertTrue(qq match {
  //     case Quoted(QuotationTag(tagUid), (QuotationBin("hello", encoder, vaseUid) *: ())) if (tagUid == vaseUid) => true
  //     case _ => false
  //   })
  //   val vase = 
  //     q.lifts.asInstanceOf[Product].productIterator.toList match {
  //       case head :: Nil => head.asInstanceOf[ScalarPlanter[String, ctx.PrepareRow /* or just Row */]]
  //     }
      
  //   assertEquals(Row("hello"), vase.encoder.apply(0, vase.value, new Row()))
  // }

  @Test
  def compiletime_liftPlusOperator() = {
    inline def q = quote {
      query[Person].map(p => p.name + lift("hello"))
    }
    assertTrue(q match {
      case Quoted(
          Map(Entity("Person", List()), Ident("p"), BinaryOperation(Property(Ident("p"), "name"), StringOperator.+, ScalarTag(tagUid))),
          (ScalarPlanter("hello", vaseUid) *: ())
        ) => true
      case _ => false
    })
  }
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

@main def noEncoderTestCompile = { 
  //import scala.language.implicitConversions
  case class Person(name: String, age: Int)

  class Foo
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  inline def q = quote {
    query[Person].map(p => p.name + lift(new Foo))
  }
  
  inline def qq = quote { //hello
    q
  }
  printer.lnf(qq)
  val output = run(qq)
}

// test a runtime quotation
// test two runtime quotations
// test a runtime going into a compiletime


// test a quotation with a context
// test a quotation producing a variable