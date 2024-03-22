package io.getquill

import io.getquill.context.mirror.{MirrorSession, Row}
import io.getquill.generic.TupleMember

class ArbitraryTupleSpec extends Spec {

  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  type MyRow1 = (Int, String)
  type MyRow2 = Int *: String *: EmptyTuple

  "ordinary tuple" in {
    inline def q = quote{
      querySchema[MyRow1]("my_table", t => t._1 -> "int_field", t => t._2 -> "string_field")
    }

    val result = ctx.run(q)

    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      (123, "St")
  }

  "arbitrary long tuple" in {
    inline def q = quote{
      querySchema[MyRow2]("my_table", t => t._1 -> "int_field", t => t._2 -> "string_field")
    }

    val result = ctx.run(q)

    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      (123, "St")
  }

}
