package io.getquill.context.sql.idiom

import io.getquill._

class PostgresDialectSpec extends OnConflictSpec with MirrorTestEntities {

  val ctx: SqlMirrorContext[PostgresDialect, Literal] =
    new SqlMirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
  import ctx._

  // "applies explicit casts" - {
  //   "toLong" in {
  //     val q = quote {
  //       qr1.map(t => t.s.toLong)
  //     }
  //     ctx.run(q).string mustEqual "SELECT t.s::bigint FROM TestEntity t"
  //   }
  //   "toInt" in {
  //     val q = quote {
  //       qr1.map(t => t.s.toInt)
  //     }
  //     ctx.run(q).string mustEqual "SELECT t.s::integer FROM TestEntity t"
  //   }
  // }

  // "Array Operations" - {
  //   case class ArrayOps(id: Int, numbers: Vector[Int])
  //   "contains" in {
  //     ctx.run(query[ArrayOps].filter(_.numbers.contains(10))).string mustEqual
  //       "SELECT x1.id, x1.numbers FROM ArrayOps x1 WHERE 10 = ANY(x1.numbers)"
  //   }
  // }

  // "prepareForProbing" in {
  //   import PostgresDialect._
  //   val id = preparedStatementId.get()

  //   prepareForProbing("SELECT t.x1, t.x2 FROM tb t WHERE (t.x1 = ?) AND (t.x2 = ?)") mustEqual
  //     s"PREPARE p${id + 1} AS SELECT t.x1, t.x2 FROM tb t WHERE (t.x1 = $$1) AND (t.x2 = $$2)"

  //   prepareForProbing("INSERT INTO tb (x1,x2,x3) VALUES (?,?,?)") mustEqual
  //     s"PREPARE p${id + 2} AS INSERT INTO tb (x1,x2,x3) VALUES ($$1,$$2,$$3)"
  // }

  "OnConflict" - {
    "no target - ignore" in {
      ctx.run(`no target - ignore`).string mustEqual
        "INSERT INTO TestEntity AS t (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
    }
    "cols target - ignore" in {
      ctx.run(`cols target - ignore`).string mustEqual
        "INSERT INTO TestEntity (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) ON CONFLICT (i) DO NOTHING"
    }

    // Should not be able to on onConflict without target in postgres (for static query, should not compile)
    "no target - update" in {
      "ctx.run(`no target - update`)" mustNot compile
    }
    "no target - update - dynamic" in {
      val q = `no target - update` // Do this to transform it into dynamic
      intercept[IllegalStateException] {
        ctx.run(q)
      }
    }

    "cols target - update" in {
      ctx.run(`cols target - update`).string mustEqual
        "INSERT INTO TestEntity AS t (s,i,l,o,b) VALUES (?, ?, ?, ?, ?) ON CONFLICT (i,s) DO UPDATE SET l = ((t.l + EXCLUDED.l) / 2), s = EXCLUDED.s"
    }
  }
}
