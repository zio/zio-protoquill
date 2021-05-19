package io.getquill.ast

import scala.language.implicitConversions

import io.getquill.Quoted
import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._
import io.getquill.quat.quatOf
import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.Spec
import io.getquill.MirrorContext
import io.getquill.MirrorSqlDialect
import io.getquill.Literal
import io.getquill.quote
import io.getquill.query
import io.getquill.EagerPlanter
import io.getquill.defaultParser

class ActionAstSpec extends Spec with Inside {
  import ShortAst._

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(id: Int, name: String, age: Int)

  "simple (assignment based apis)" - {
    "simple insert" in {

    }
    "simple update Step 1" in {
      inline def q = quote { query[Person].update(_.name -> lift("Joe"), _.age -> 123) }
      //println( io.getquill.util.Messages.qprint(q) )
      //println( ctx.run(q) )
    }
    "simple update Step 2" in {
      inline def q = quote { query[Person].update(_.name -> lift("Joe"), _.age -> 123) }
      //println( io.getquill.util.Messages.qprint(q) )
      q must matchPattern {
        case Quoted(
          Update(
            Ent("Person"),
            List(
              Assignment(Id("x3"), Property(Id("x3"), "name"), ScalarTag(nameId1)),
              Assignment(Id("x4"), Property(Id("x4"), "age"), Constant(123, _))
            )
          ), 
          List(EagerPlanter("Joe", encoder, nameId2)), 
          Nil
        ) if (nameId1 == nameId2) =>
      }
      ctx.run(q).triple mustEqual ("UPDATE Person SET name = ?, age = 123", List("Joe"), ExecutionType.Static)
    }
    "simple delete" in {

    }
  }













  

} 