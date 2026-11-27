package zio.quill.customparser

import scala.language.implicitConversions
import zio.quill.ast._
import zio.quill.Quoted
import zio.quill.customparser.CustomParser
import zio.quill.customparser.CustomOps
import zio.quill.{ defaultParser => _, _ }

import org.scalatest._

class CustomParseTest extends Spec with Inside {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._ //

  "can customize parsing spec" in {
    given myParser: CustomParser.type = CustomParser
    import CustomOps._
    case class Person(name: String, age: Int)
    inline def q = quote { query[Person].map(p => p.age ** 2) }
    ctx.run(q).string mustEqual "SELECT power(p.age ,2) FROM Person p"
  }

}