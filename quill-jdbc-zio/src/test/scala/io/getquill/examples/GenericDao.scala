package io.getquill.examples

import io.getquill._
import reflect.Selectable.reflectiveSelectable
import io.getquill.jdbczio.Quill.DataSource
import javax.sql.DataSource
import scala.tools.util.PathResolver.Environment
import zio.ZEnvironment.apply
import zio.ZEnvironment
import io.getquill.jdbczio.Quill
import zio.ZIO
import zio.ZIOAppDefault
import zio.Console.printLine

object QuillPostgres extends PostgresZioJdbcContext(Literal)
import QuillPostgres._

case class PersonT(id: Int, first: String, last: String, age: Int)

class Repo[T <: { def id: Int }](ds: DataSource) {
  val env = ZEnvironment(ds)

  inline def getById(inline id: Int) =
    run(query[T].filter(t => t.id == lift(id))).map(_.headOption).provideEnvironment(env)

  inline def insert(inline t: T) =
    run(query[T].insertValue(lift(t)).returning(_.id)).provideEnvironment(env)

  inline def searchByField(inline predicate: T => Boolean) =
    run(query[T].filter(p => predicate(p))).provideEnvironment(env)
}

class PeopleRepo(ds: DataSource) extends Repo[PersonT](ds)
object PeopleRepo {
  def live = ZIO.serviceWith(new PeopleRepo(_))
}

object StructureBasedRepo extends ZIOAppDefault {
  import QuillPostgres._

  val joe = PersonT(123, "Joe", "Bloggs", 123)

  def run =
    (for {
      peopleRepo <- PeopleRepo.live
      joeId      <- peopleRepo.insert(joe)
      joeNew     <- peopleRepo.getById(joeId)
      allJoes    <- peopleRepo.searchByField(p => p.first == "Joe")
      _          <-
        printLine("==== joe: " + joe) *>
        printLine("==== joeId: " + joeId) *>
        printLine("==== joeNew: " + joeNew) *>
        printLine("==== allJoes: " + allJoes)
    } yield ()).provide(
      Quill.DataSource.fromPrefix("testPostgresDB")
    )
}
