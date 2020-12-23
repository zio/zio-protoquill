package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.Dsl
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

class QuerySchemaTest extends Spec with Inside {

  case class TestEntity(s: String, i: Int, l: Long, o: Option[Int])
  case class TestEntity2(s: String, i: Int, l: Long, o: Option[Int])
  case class TestEntity3(s: String, i: Int, l: Long, o: Option[Int])
  case class TestEntity4(i: Long)
  case class TestEntity5(s: String, i: Long)

  val ctx = new MirrorContext(MirrorIdiom, Literal)
  import ctx._

  // inline def verifyQuoteAndRun[T](implicit inline meta: SchemaMeta[T]) = {
  //   inline val q = quote(query[T])

  // }

  implicit class QueryMirrorExt[T](qm: QueryMirror[T]) {
    def strAndExec = (qm.string, qm.executionType)
  }

  case class EmbValue(i: Int) extends Embedded

  // "schema meta lookup" - { //hellooooo
  //   case class TestEntity(s: String, i: Int, l: Long, ooo: Option[Int])
  //   case class Person(name: String, age: Int)
  //   //inline using SchemaMeta[Person] = schemaMeta[Person]("tblPerson", _.name -> "colName", _.age -> "colAge")
  //   //inline using meta: SchemaMeta[TestEntity] = schemaMeta[TestEntity]("test_entity", _.i -> "ii")
  //   inline def q = quote { query[TestEntity] } //.map(p => p.name)
  //   printer.lnf(q.ast)

  //   //val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  //   //import ctx._
  //   println(ctx.run(q).string)
  // }

  "schema meta" - {
    // "materialized" in {
    //   //inline def meta = materializeSchemaMeta[TestEntity] // High/Low Implicit Priority is not even needed anymore, yay!
    //   inline def q = quote(query[TestEntity])
    //   q.ast.toString mustEqual """querySchema("TestEntity")"""
    //   ctx.run(q).strAndExec mustEqual ("""querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Static)
    // }
    "custom" in { //hello
      implicit inline def meta: SchemaMeta[TestEntity] = schemaMeta("test_entity", _.i -> "ii")
      inline def q = quote(query[TestEntity])
      q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Static)
    }
    "custom-idiomatic" in { //hello
      inline given sm: SchemaMeta[TestEntity] = schemaMeta("test_entity", _.i -> "ii")
      inline def q = quote(query[TestEntity])
      q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Static)
    }
    // using dynamic SchemaMeta must be possible as well
    "custom dynamic-meta/static-query" in {
      implicit val meta: SchemaMeta[TestEntity] = schemaMeta[TestEntity]("test_entity", _.i -> "ii")
      inline def q = quote(query[TestEntity])
      printer.lnf(q.ast)
      println(q.ast)
      //q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Dynamic)
    }
    "custom dynamic-meta/static-query - idiomatic" in {
      implicit val meta: SchemaMeta[TestEntity] = schemaMeta[TestEntity]("test_entity", _.i -> "ii")
      inline def q = quote(query[TestEntity])
      printer.lnf(q.ast)
      println(q.ast)
      //q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Dynamic)
    }
    "custom dynamic meta with dynamic query" in {
      implicit val meta: SchemaMeta[TestEntity] = schemaMeta[TestEntity]("test_entity", _.i -> "ii")
      def q = quote(query[TestEntity])
      printer.lnf(q.ast)
      println(q.ast)  
      //q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Dynamic)
    }
    // TODO Need to enhance parser to do this
    // "custom dynamic and composition" in {
    //   implicit val meta: SchemaMeta[TestEntity] = schemaMeta[TestEntity]("test_entity", _.i -> "ii")
    //   inline def q = quote(query[TestEntity].filter(e => e.i == 1))
    //   printer.lnf(q.ast)
    //   println(q.ast)
    //   //q.ast.toString mustEqual """querySchema("test_entity", _.i -> "ii")"""
    //   ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.i -> "ii").map(x => (x.s, x.i, x.l, x.o))""", ExecutionType.Dynamic)
    // }
    "custom with embedded" in {
      case class Entity(emb: EmbValue)
      implicit inline def meta: SchemaMeta[Entity] = schemaMeta[Entity]("test_entity", _.emb.i -> "ii")
      inline def q = quote(query[Entity])
      q.ast.toString mustEqual """querySchema("test_entity", _.emb.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.emb.i -> "ii").map(x => x.emb.i)""", ExecutionType.Static)
    }
    "custom with optional embedded" in {
      case class Entity(emb: Option[EmbValue])
      implicit inline def meta: SchemaMeta[Entity] = schemaMeta[Entity]("test_entity", _.emb.map(_.i) -> "ii")
      inline def q = quote(query[Entity])
      q.ast.toString mustEqual """querySchema("test_entity", _.emb.i -> "ii")"""
      ctx.run(q).strAndExec mustEqual ("""querySchema("test_entity", _.emb.i -> "ii").map(x => x.emb.map((v) => v.i))""", ExecutionType.Static)
    }
  }
  
}