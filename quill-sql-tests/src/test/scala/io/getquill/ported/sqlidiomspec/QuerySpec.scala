package io.getquill.ported.sqlidiomspec

import scala.language.implicitConversions
import io.getquill.ReturnAction.ReturnColumns
//import io.getquill.MirrorSqlDialectWithReturnMulti
import io.getquill.context.mirror.Row
import io.getquill.context.sql.testContext
import io.getquill.context.sql.testContext._
import io.getquill._

class QuerySpec extends Spec {
  "query" - {
    "without filter" in {
      testContext.run(qr1).string mustEqual
        "SELECT x.s, x.i, x.l, x.o, x.b FROM TestEntity x"
    }
    "with filter" in {
      inline def q = quote {
        qr1.filter(t => t.s == "s")
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s = 's'"
    }
    "multiple filters" - {
      "ANDs" in {
        inline def q = quote {
          qr1.filter(t => t.s == "a").filter(t => t.i == 1).filter(t => t.l == 2L)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s = 'a' AND t.i = 1 AND t.l = 2"
      }
      "ORs" in {
        inline def q = quote {
          qr1.filter(t => t.s == "a" || t.i == 1 || t.i == 2)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s = 'a' OR t.i = 1 OR t.i = 2"
      }
      "ANDs and ORs" in {
        inline def q = quote {
          qr1.filter(t => t.s == "a" && t.i == 1 || t.i == 2)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s = 'a' AND t.i = 1 OR t.i = 2"
      }

      "ensure precedence" - {
        "OR AND OR" in {
          inline def q = quote {
            qr1.filter(t => t.s == "a" || t.s == "x").filter(t => t.i == 1 || t.i == 2)
          }
          testContext.run(q).string mustEqual
            "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE (t.s = 'a' OR t.s = 'x') AND (t.i = 1 OR t.i = 2)"
        }
        "AND + scoped ORs" in {
          inline def q = quote {
            qr1.filter(t => t.s == "s").filter(t => t.i == 1 || t.i == 2 || t.i == 3)
          }
          testContext.run(q).string mustEqual
            "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s = 's' AND (t.i = 1 OR t.i = 2 OR t.i = 3)"
        }
        "scoped ORs + AND" in {
          inline def q = quote {
            qr1.filter(t => t.i == 1 || t.i == 2 || t.i == 3).filter(t => t.s == "s")
          }
          testContext.run(q).string mustEqual
            "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE (t.i = 1 OR t.i = 2 OR t.i = 3) AND t.s = 's'"
        }
      }
    }

    "multiple entities" in {
      inline def q = quote {
        for {
          a <- qr1
          b <- qr2 if a.s == b.s
        } yield {
          a.s
        }
      }
      testContext.run(q).string mustEqual
        "SELECT a.s FROM TestEntity a, TestEntity2 b WHERE a.s = b.s"
    }
    "concatMap + split" in {
      inline def q = quote {
        qr1.concatMap(t => t.s.split(" "))
      }
      testContext.run(q).string mustEqual
        "SELECT UNNEST(SPLIT(t.s, ' ')) FROM TestEntity t"
    }
    "startsWith" in {
      inline def q = quote {
        qr1.filter(t => t.s.startsWith(" "))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s LIKE (' ' || '%')"
    }
    "distinct" - {
      "simple" in {
        inline def q = quote {
          qr1.distinct
        }
        testContext.run(q).string mustEqual
          "SELECT x.s, x.i, x.l, x.o, x.b FROM (SELECT DISTINCT x.s, x.i, x.l, x.o, x.b FROM TestEntity x) AS x"
      }

      "single" in {
        inline def q = quote {
          qr1.map(i => i.i).distinct
        }
        testContext.run(q).string mustEqual
          "SELECT DISTINCT i.i FROM TestEntity i"
      }
      // A little different from Scala2-Quill Since in ProtoQuill Elaborates to a ast.CaseClass not a ast.Tuple
      "tuple" in {
        inline def q = quote {
          qr1.map(i => (i.i, i.l)).distinct
        }
        testContext.run(q).string mustEqual
          "SELECT i._1, i._2 FROM (SELECT DISTINCT i.i AS _1, i.l AS _2 FROM TestEntity i) AS i"
      }

      // A little different from Scala2-Quill Since in ProtoQuill Elaborates to a ast.CaseClass not a ast.Tuple
      "caseclass constructor" in {
        case class IntLong(i: Int, l: Long)
        inline def q = quote {
          qr1.map(i => new IntLong(i.i, i.l)).distinct
        }
        testContext.run(q).string mustEqual
          "SELECT DISTINCT i.i, i.l FROM TestEntity i"
      }

      // A little different from Scala2-Quill Since in ProtoQuill Elaborates to a ast.CaseClass not a ast.Tuple
      "caseclass companion constructor" in {
        case class IntLong(i: Int, l: Long)
        inline def q = quote {
          qr1.map(i => IntLong(i.i, i.l)).distinct
        }
        testContext.run(q).string mustEqual
          "SELECT DISTINCT i.i, i.l FROM TestEntity i"
      }

      "nesting" in {
        inline def q = quote {
          qr1.map(i => i.i).distinct.map(x => x + 1)
        }
        testContext.run(q).string mustEqual
          "SELECT i.i + 1 FROM (SELECT DISTINCT i.i FROM TestEntity i) AS i"
      }

      "with join + filter" in {
        inline def q = quote {
          for {
            v1 <- qr1.map(i => i.i).distinct
            v2 <- qr2.filter(_.i == v1)
          } yield (v1, v2)
        }
        testContext.run(q).string mustEqual
          "SELECT i.i, x1.s, x1.i, x1.l, x1.o FROM (SELECT DISTINCT i.i FROM TestEntity i) AS i, TestEntity2 x1 WHERE x1.i = i.i"
      }

      "with two joins" in {
        inline def q = quote {
          for {
            v1 <- qr1.map(i => i.i).distinct
            v2 <- qr2.sortBy(_.l).join(_.i == v1)
          } yield (v1, v2)
        }
        testContext.run(q).string mustEqual
          "SELECT i.i, x2.s, x2.i, x2.l, x2.o FROM (SELECT DISTINCT i.i FROM TestEntity i) AS i INNER JOIN (SELECT x2.s, x2.i, x2.l, x2.o FROM TestEntity2 x2 ORDER BY x2.l ASC NULLS FIRST) AS x2 ON x2.i = i.i"
      }

      // Requires run(QuerySingle) not implemented yet
      // "followed by aggregation" in {
      //   inline def q = quote {
      //     qr1.map(i => i.i).distinct.size
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT COUNT(*) FROM (SELECT DISTINCT i.i FROM TestEntity i) AS x"
      // }
    }
    "sorted" - {
      "simple" in {
        inline def q = quote {
          qr1.filter(t => t.s != null).sortBy(_.s)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s IS NOT NULL ORDER BY t.s ASC NULLS FIRST"
      }
      "nested" in {
        inline def q = quote {
          for {
            a <- qr1.sortBy(t => t.s)
            b <- qr2.sortBy(t => t.i)
          } yield {
            (a.s, b.i)
          }
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t1.i FROM (SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s ASC NULLS FIRST) AS t, (SELECT t1.s, t1.i, t1.l, t1.o FROM TestEntity2 t1 ORDER BY t1.i ASC NULLS FIRST) AS t1"
      }
      "asc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.asc)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s ASC"
      }
      "desc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.desc)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s DESC"
      }
      "ascNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsFirst)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s ASC NULLS FIRST"
      }
      "descNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsFirst)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s DESC NULLS FIRST"
      }
      "ascNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsLast)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s ASC NULLS LAST"
      }
      "descNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsLast)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.s DESC NULLS LAST"
      }
      "tuple" in {
        inline def q = quote {
          qr1.sortBy(t => (t.i, t.s))(Ord(Ord.desc, Ord.asc))
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY t.i DESC, t.s ASC"
      }
      "expression" in {
        inline def q = quote {
          qr1.sortBy(t => t.i * 3)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t ORDER BY (t.i * 3) ASC NULLS FIRST"
      }
    }
    "grouped" - {
      "simple" in {
        inline def q = quote {
          qr1.groupBy(t => t.i).map {
            case (i, entities) => (i, entities.size)
          }
        }
        testContext.run(q).string mustEqual
          "SELECT t.i, COUNT(*) FROM TestEntity t GROUP BY t.i"
      }
      "nested" in {
        inline def q = quote {
          for {
            (a, b) <- qr1.groupBy(t => t.i).map {
              case (i, entities) => (i, entities.size)
            }
            c <- qr2 if c.i == a
          } yield {
            (a, b, c)
          }
        }
        testContext.run(q).string mustEqual
          "SELECT t._1, t._2, c.s, c.i, c.l, c.o FROM (SELECT t.i AS _1, COUNT(*) AS _2 FROM TestEntity t GROUP BY t.i) AS t, TestEntity2 c WHERE true AND c.i = t._1"
      }
      "limited" in {
        inline def q = quote {
          qr1.groupBy(t => t.i).map {
            case (i, e) =>
              (i, e.map(_.l).min)
          }.take(10)
        }

        testContext.run(q).string mustEqual
          "SELECT t._1, t._2 FROM (SELECT t.i AS _1, MIN(t.l) AS _2 FROM TestEntity t GROUP BY t.i) AS t LIMIT 10"
      }
      "filter.flatMap(groupBy)" in {
        inline def q = quote {
          for {
            a <- qr1 if a.i == 1
            b <- qr2.groupBy(t => t.i).map { case _ => 1 }
          } yield b
        }
        testContext.run(q).string mustEqual
          "SELECT t.* FROM TestEntity a, (SELECT 1 FROM TestEntity2 t GROUP BY t.i) AS t WHERE a.i = 1"
      }
    }
    "aggregated" - {
      // Requires run(QuerySingle) not implemented yet
      // "min" in {
      //   inline def q = quote {
      //     qr1.map(t => t.i).min
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT MIN(t.i) FROM TestEntity t"
      // }
      // Requires run(QuerySingle) not implemented yet
      // "max" in {
      //   inline def q = quote {
      //     qr1.map(t => t.i).max
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT MAX(t.i) FROM TestEntity t"
      // }
      // Requires run(QuerySingle) not implemented yet
      // "avg" in {
      //   inline def q = quote {
      //     qr1.map(t => t.i).avg
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT AVG(t.i) FROM TestEntity t"
      // }
      // Requires run(QuerySingle) not implemented yet
      // "sum" in {
      //   inline def q = quote {
      //     qr1.map(t => t.i).sum
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT SUM(t.i) FROM TestEntity t"
      // }
      "size" - {
        // Requires run(QuerySingle) not implemented yet
        // "regular" in {
        //   inline def q = quote {
        //     qr1.size
        //   }
        //   testContext.run(q).string mustEqual
        //     "SELECT COUNT(*) FROM TestEntity x"
        // }
        "with groupBy" in {
          inline def q = quote {
            qr1.map(t => (t.i, t.s)).groupBy(t => t._1).map {
              case (i, l) => l.size
            }
          }
          testContext.run(q).string mustEqual
            "SELECT COUNT(*) FROM TestEntity t GROUP BY t.i"
        }
      }
      // Requires run(QuerySingle) not implemented yet
      // "with filter" in {
      //   inline def q = quote {
      //     qr1.filter(t => t.i > 1).map(t => t.i).min
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT MIN(t.i) FROM TestEntity t WHERE t.i > 1"
      // }
      "as select value" in {
        inline def q = quote {
          qr1.take(10).map(a => qr2.filter(t => t.i > a.i).map(t => t.i).min)
        }
        testContext.run(q).string mustEqual
          "SELECT (SELECT MIN(t.i) FROM TestEntity2 t WHERE t.i > a.i) FROM TestEntity a LIMIT 10"
      }
      // Requires run(QuerySingle) not implemented yet
      // "after a group by" in {
      //   inline def q = quote {
      //     qr1.groupBy(t => t.s).map { case (a, b) => (a, b.size) }.size
      //   }
      //   testContext.run(q).string mustEqual
      //     "SELECT COUNT(*) FROM TestEntity t GROUP BY t.s"
      // }
      "group by + binary op select" in {
        inline def q = quote {
          qr1.groupBy(t => t.i).map {
            case (i, list) => (i, list.size + 1)
          }
        }
        testContext.run(q).string mustEqual
          "SELECT t.i, (COUNT(*)) + 1 FROM TestEntity t GROUP BY t.i"
      }
    }
    // Requires run(QuerySingle) not implemented yet
    // "unary operation" - {
    //   "nonEmpty" in {
    //     testContext.run(qr1.nonEmpty).string mustEqual
    //       "SELECT EXISTS (SELECT x.s, x.i, x.l, x.o, x.b FROM TestEntity x)"
    //   }
    //   "isEmpty" in {
    //     testContext.run(qr1.isEmpty).string mustEqual
    //       "SELECT NOT EXISTS (SELECT x.s, x.i, x.l, x.o, x.b FROM TestEntity x)"
    //   }
    // }
    "limited" - {
      "simple" in {
        inline def q = quote {
          qr1.filter(t => t.s != null).take(10)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.s IS NOT NULL LIMIT 10"
      }
      "nested" in {
        inline def q = quote {
          for {
            a <- qr1.take(1)
            b <- qr2.take(2)
          } yield {
            (a.s, b.i)
          }
        }
        testContext.run(q).string mustEqual
          "SELECT a.s, b.i FROM (SELECT x.s, x.i, x.l, x.o, x.b FROM TestEntity x LIMIT 1) AS a, (SELECT x.s, x.i, x.l, x.o FROM TestEntity2 x LIMIT 2) AS b"
      }
    }
    "union" - {
      "simple" in {
        inline def q = quote {
          qr1.filter(t => t.i > 10).union(qr1.filter(t => t.s == "s"))
        }
        testContext.run(q).string mustEqual
          "SELECT x.s, x.i, x.l, x.o, x.b FROM ((SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i > 10) UNION (SELECT t1.s, t1.i, t1.l, t1.o, t1.b FROM TestEntity t1 WHERE t1.s = 's')) AS x"
      }
      "mapped" in {
        inline def q = quote {
          qr1.filter(t => t.i > 10).map(u => u).union(qr1.filter(t => t.s == "s")).map(u => u.s)
        }
        testContext.run(q).string mustEqual
          "SELECT u.s FROM ((SELECT t.s FROM TestEntity t WHERE t.i > 10) UNION (SELECT t1.s FROM TestEntity t1 WHERE t1.s = 's')) AS u"
      }
      "nested" in {
        val j = quote {
          for {
            a <- qr1
            b <- qr2
          } yield {
            (a, b)
          }
        }
        inline def q = quote {
          j.union(j).map(u => (u._1.s, u._2.i))
        }
        testContext.run(q).string mustEqual
          "SELECT u._1s, u._2i FROM ((SELECT a.s AS _1s, b.i AS _2i FROM TestEntity a, TestEntity2 b) UNION (SELECT a1.s AS _1s, b1.i AS _2i FROM TestEntity a1, TestEntity2 b1)) AS u"
      }
    }
    "unionAll" - {
      "simple" in {
        inline def q = quote {
          qr1.filter(t => t.i > 10).unionAll(qr1.filter(t => t.s == "s"))
        }
        testContext.run(q).string mustEqual
          "SELECT x.s, x.i, x.l, x.o, x.b FROM ((SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i > 10) UNION ALL (SELECT t1.s, t1.i, t1.l, t1.o, t1.b FROM TestEntity t1 WHERE t1.s = 's')) AS x"
      }
    }
    "join" - {
      "inner" in {
        inline def q = quote {
          qr1.join(qr2).on((a, b) => a.s == b.s).map(_._1)
        }
        testContext.run(q).string mustEqual
          "SELECT a.s, a.i, a.l, a.o, a.b FROM TestEntity a INNER JOIN TestEntity2 b ON a.s = b.s"
      }
      "left" in {
        inline def q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.s == b.s).map(_._1)
        }
        testContext.run(q).string mustEqual
          "SELECT a.s, a.i, a.l, a.o, a.b FROM TestEntity a LEFT JOIN TestEntity2 b ON a.s = b.s"
      }
      "right" in {
        inline def q = quote {
          qr1.rightJoin(qr2).on((a, b) => a.s == b.s).map(_._2)
        }
        testContext.run(q).string mustEqual
          "SELECT b.s, b.i, b.l, b.o FROM TestEntity a RIGHT JOIN TestEntity2 b ON a.s = b.s"
      }
      "full" in {
        inline def q = quote {
          qr1.fullJoin(qr2).on((a, b) => a.s == b.s).map(_._1.map(c => c.s))
        }
        testContext.run(q).string mustEqual
          "SELECT a.s FROM TestEntity a FULL JOIN TestEntity2 b ON a.s = b.s"
      }
      "multiple outer joins" in {
        inline def q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.s == b.s).leftJoin(qr2).on((a, b) => a._1.s == b.s).map(_._1._1)
        }
        testContext.run(q).string mustEqual
          "SELECT a.s, a.i, a.l, a.o, a.b FROM TestEntity a LEFT JOIN TestEntity2 b ON a.s = b.s LEFT JOIN TestEntity2 b1 ON a.s = b1.s"
      }
      "with flatMap" - {
        "left" ignore {
          // TODO flatten left flatMaps
          inline def q = quote {
            qr1.flatMap(a => qr2).leftJoin(qr3).on((b, c) => b.s == c.s).map(_._1)
          }
          testContext.run(q).string mustEqual ""
        }
        "right" in {
          inline def q = quote {
            qr1.leftJoin(qr2).on((a, b) => a.s == b.s).flatMap(c => qr3)
          }
          testContext.run(q).string mustEqual
            "SELECT x.s, x.i, x.l, x.o FROM TestEntity a LEFT JOIN TestEntity2 b ON a.s = b.s, TestEntity3 x"
        }
      }
      "with map" - {
        "left" in {
          inline def q = quote {
            qr1.map(y => y.s).join(qr2).on((a, b) => a == b.s)
          }
          testContext.run(q).string mustEqual
            "SELECT y.s, b.s, b.i, b.l, b.o FROM TestEntity y INNER JOIN TestEntity2 b ON y.s = b.s"
        }
      }
    }
    // TODO Requires run(QuerySingle) which is not implemented yet
    // "without from" in {
    //   inline def q = quote {
    //     qr1.map(t => t.i).size == 1L
    //   }
    //   testContext.run(q).string mustEqual
    //     "SELECT ((SELECT COUNT(t.i) FROM TestEntity t)) = 1"
    // }
    "contains" in {
      inline def q = quote {
        qr1.filter(t => qr2.map(p => p.i).contains(t.i))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i IN (SELECT p.i FROM TestEntity2 p)"
    }

    "set" - {
      "non-empty" in {
        inline def q = quote {
          qr1.filter(t => liftQuery(Set(1, 2, 3)).contains(t.i))
        }
        testContext.run(q).string mustEqual "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i IN (?, ?, ?)"
      }
      "empty" in {
        inline def q = quote {
          qr1.filter(t => liftQuery(Set.empty[Int]).contains(t.i))
        }
        testContext.run(q).string mustEqual "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE FALSE"
      }
      "empty dynamic" in { // (adding this since dynamic empty set uses different logic then the static one)
        val q = quote {
          qr1.filter(t => liftQuery(Set.empty[Int]).contains(t.i))
        }
        testContext.run(q).string mustEqual "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE FALSE"
      }
    }
  }
}