package io.getquill

import io.getquill.context.ExecutionType.Static
import io.getquill.context.mirror.{MirrorSession, Row}
import io.getquill.generic.TupleMember

class ArbitraryTupleSpec extends Spec {

  val ctx = new MirrorContext(PostgresDialect, Literal)
  import ctx._

  type MyRow1 = (Int, String)
  type MyRow2 = Int *: String *: EmptyTuple

  inline def myRow1Query = quote {
    querySchema[MyRow1]("my_table", t => t._1 -> "int_field", t => t._2 -> "string_field")
  }

  inline def myRow2Query = quote {
    querySchema[MyRow2]("my_table", t => t._1 -> "int_field", t => t._2 -> "string_field")
  }

  "ordinary tuple" in {
    val result = ctx.run(myRow1Query)

    result.string mustEqual "SELECT x.int_field, x.string_field FROM my_table x"
    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      (123, "St")
  }

  "ordinary tuple swap" in {

    transparent inline def swapped: Quoted[EntityQuery[(String, Int)]] = quote {
      myRow1Query.map {
        case (i, s) => (s, i)
      }
    }

    val result = ctx.run(swapped)

    result.string mustEqual "SELECT x$1.string_field AS _1, x$1.int_field AS _2 FROM my_table x$1"
    require(result.extractor(Row("St", 123), MirrorSession.default) == ("St", 123))
  }

  "arbitrary long tuple" in {
    val result = ctx.run(myRow2Query)

    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      (123, "St")
  }

  "get field of arbitrary long tuple" in {
    inline def g = quote{
      myRow2Query.map{
        case h *: tail => h
      }
    }
    val result = ctx.run(g)

    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      (123)
  }

  "decode empty tuple" in {
    inline def g = quote {
      myRow2Query.map{
        case (_, _) => EmptyTuple
      }
    }

    val result = ctx.run(g)

    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual EmptyTuple
  }

  "construct tuple1" in {
    inline def g = quote {
      myRow1Query.map {
        case (i, s) => i *: EmptyTuple
      }
    }

    val result = ctx.run(g)

    result.string mustEqual "SELECT x$1.int_field AS _1 FROM my_table x$1"
    result.extractor(Row(123, "St"), MirrorSession.default) mustEqual
      Tuple1(123)
  }

  "construct arbitrary tuple" in {
    inline def g = quote {
      myRow1Query.map {
        case (i, s) => s *: i *: EmptyTuple
      }
    }
    val result = ctx.run(g)

    result.string mustEqual "SELECT x$1.string_field AS _1, x$1.int_field AS _2 FROM my_table x$1"
    result.extractor(Row("St", 123), MirrorSession.default) mustEqual ("St", 123)

  }

  "constant arbitrary tuple" in {
    inline def g = quote {
      123 *: "St" *: true *: (3.14 *: EmptyTuple)
    }
    val result = ctx.run(g)
    result.string mustEqual "SELECT 123 AS _1, 'St' AS _2, true AS _3, 3.14 AS _4"
    result.info.executionType mustEqual Static
    result.extractor(Row(123, "St", true, 3.14), MirrorSession.default) mustEqual (123, "St", true, 3.14)
  }

  "constant arbitrary tuple 1" in {
    inline def g = quote {
      (3.14 *: EmptyTuple)
    }
    val result = ctx.run(g)
    result.string mustEqual "SELECT 3.14 AS _1"
    result.info.executionType mustEqual Static
    result.extractor(Row(3.14), MirrorSession.default) mustEqual Tuple1(3.14)
  }
}
