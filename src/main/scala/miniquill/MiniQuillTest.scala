package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.QueryDsl._

object AdvTest {
  import io.getquill._

  case class Country(id: Int, name: String, population: Int)
  case class Company(id: Int, name: String, countryId: Int)
  case class Person(id: Int, name: String, age: Int, companyId: Int)
  case class ClientAffiliation(host: Int, of: Int)

  inline def q = quote {
    for {
      o1 <- query[Country]
      c1 <- query[Company].join(c1 => c1.countryId == o1.id)
      p1 <- query[Person].join(p1 => p1.companyId == c1.id)
      af <- query[ClientAffiliation].leftJoin(af => af.of == p1.id)
      p2 <- query[Person].leftJoin(p2 => af.map(_.host).exists(v => v == p2.id))
    } yield (o1, c1, p1, af, p2)
  }

  def main(args: Array[String]): Unit = {
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._
    println( run(q) )


  }
}
