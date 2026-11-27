package zio.quill

import zio.quill.context.sql.PeopleSpec
import zio.quill.context.qzio.ZioJdbcContext
import zio.quill.jdbczio.Quill

trait PeopleZioSpec extends PeopleSpec with ZioSpec {

  val context: Quill[_, _]
  import context._

  inline def `Ex 11 query` = quote(query[Person])
  val `Ex 11 expected` = peopleEntries
}

trait PeopleZioProxySpec extends PeopleSpec with ZioProxySpec {

  val context: ZioJdbcContext[_, _]
  import context._

  val `Ex 11 query` = quote(query[Person])
  val `Ex 11 expected` = peopleEntries
}
