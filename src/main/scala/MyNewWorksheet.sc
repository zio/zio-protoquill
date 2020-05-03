import io.getquill._
import scala.language.implicitConversions
import miniquill.quoter.CoreDsl._

val ctx = new MirrorContext(MirrorSqlDialect, Literal)
import ctx._

case class Person(id: Int, name: String)
case class Address(street: String, ownerId: Int)

inline def q = quote {
  query[Person].map(p => p.name)
}

import printer.AstPrinter
println(new AstPrinter()(q.ast).plainText)

println(new AstPrinter()(q).plainText)
