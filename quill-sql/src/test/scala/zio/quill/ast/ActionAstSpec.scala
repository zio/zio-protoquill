package zio.quill.ast

import scala.language.implicitConversions

import zio.quill.Quoted
import zio.quill.ast._
import zio.quill.QuotationLot
import zio.quill.QuotationVase
import zio.quill.context.ExecutionType
import org.scalatest._
import zio.quill.quat.quatOf
import zio.quill.context.ExecutionType.Static
import zio.quill.context.ExecutionType.Dynamic
import zio.quill.Spec
import zio.quill.MirrorContext
import zio.quill.MirrorSqlDialect
import zio.quill.Literal
import zio.quill.quote
import zio.quill.query
import zio.quill.EagerPlanter
import zio.quill.defaultParser

class ActionAstSpec extends Spec with Inside {
  import ShortAst._

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(id: Int, name: String, age: Int)

  "simple (assignment based apis)" - {
    "simple insert" in {}
    "simple update Step 1" in {
      inline def q = quote { query[Person].update(_.name -> lift("Joe"), _.age -> 123) }
      // println( zio.quill.util.Messages.qprint(q) )
      // println( ctx.run(q) )
    }
    "simple update Step 2" in {
      inline def q = quote { query[Person].update(_.name -> lift("Joe"), _.age -> 123) }
      // println( zio.quill.util.Messages.qprint(q) )
      q must matchPattern {
        case Quoted(
              Update(
                Ent("Person"),
                List(
                  Assignment(Id("x3"), Property(Id("x3"), "name"), ScalarTag(nameId1, _)),
                  Assignment(Id("x4"), Property(Id("x4"), "age"), Constant(123, _))
                )
              ),
              List(EagerPlanter("Joe", encoder, nameId2)),
              Nil
            ) if (nameId1 == nameId2) =>
      }
      ctx.run(q).triple mustEqual ("UPDATE Person SET name = ?, age = 123", List("Joe"), ExecutionType.Static)
    }
    "simple delete" in {}
  }

}
