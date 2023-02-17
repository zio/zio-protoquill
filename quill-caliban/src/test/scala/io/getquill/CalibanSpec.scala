package io.getquill

import caliban.CalibanError.{ExecutionError, ParsingError, ValidationError}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import zio.{Task, ZIO}
import caliban.{CalibanError, GraphQL}
import io.getquill.jdbczio.Quill
import io.getquill.util.ContextLogger

trait CalibanSpec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  object Ctx extends PostgresZioJdbcContext(Literal)
  import Ctx._
  lazy val zioDS = Quill.DataSource.fromPrefix("testPostgresDB")

  private val logger = ContextLogger(this.getClass)

  // FlatSchema and NestedSchema share the same DB data so only need to create it using one of them
  override def beforeAll() = {
    import FlatSchema._
    (for { //
      _ <- Ctx.run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]])
      _ <- Ctx.run(liftQuery(ExampleData.people).foreach(row => query[PersonT].insertValue(row)))
      _ <- Ctx.run(liftQuery(ExampleData.addresses).foreach(row => query[AddressT].insertValue(row)))
    } yield ()).provideLayer(zioDS).unsafeRunSync()
  }

  // override def afterAll() = {
  //   import FlatSchema._
  //   Ctx.run(sql"TRUNCATE TABLE AddressT, PersonT RESTART IDENTITY".as[Delete[PersonT]]).provideLayer(zioDS).unsafeRunSync()
  // }

  def api: GraphQL[Any]

  extension [A](qzio: ZIO[Any, Throwable, A])
    def unsafeRunSync(): A =
      zio.Unsafe.unsafe { implicit unsafe =>
        zio.Runtime.default.unsafe.run(qzio).getOrThrow()
      }

  def unsafeRunQuery(queryString: String) =
    val output =
      (for {
        interpreter <- api.interpreter
        result <- interpreter.execute(queryString)
      } yield (result)).tapError { e =>
        fail("GraphQL Validation Error", e)
        ZIO.unit
      }.unsafeRunSync()

    def printMessage(prefix: String, msg: String, cause: Option[Throwable]) = {
      import io.getquill.util.CommonExtensions.Throwable._
      s"${prefix}: ${msg}" +
        (cause match {
          case Some(e) =>
            s"\n===================\n${e.stackTraceToString}"
          case None =>
            ""
        })
    }

    if (output.errors.length != 0)
      fail(s"GraphQL Validation Failures:\n${output.errors.map { (calibanError: CalibanError) =>
        calibanError match {
          case v: ParsingError    => printMessage("Caliban ParsingError: ", v.msg, v.innerThrowable)
          case v: ValidationError => printMessage("Caliban ValidationError: ", v.msg, None)
          case v: ExecutionError  => printMessage("Caliban ExecutionError: ", v.msg, v.innerThrowable)
        }
      }}")
    else
      output.data.toString
  end unsafeRunQuery
}
