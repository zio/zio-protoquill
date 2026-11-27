package zio.quill.context.jdbc

import java.sql.ResultSet
import java.sql.Connection
import scala.annotation.tailrec

// TODO In the Scala 2 Quill, move this module to portable
object ResultSetExtractor {

  private[quill] final def apply[T](rs: ResultSet, conn: Connection, extractor: (ResultSet, Connection) => T): List[T] =
    extractResult(rs, conn, extractor, List())

  @tailrec
  private[quill] final def extractResult[T](rs: ResultSet, conn: Connection, extractor: (ResultSet, Connection) => T, acc: List[T]): List[T] =
    if (rs.next)
      extractResult(rs, conn, extractor, extractor(rs, conn) :: acc)
    else
      acc.reverse
}
