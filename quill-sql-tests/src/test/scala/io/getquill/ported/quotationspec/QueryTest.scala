package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill._
import io.getquill.ast.Renameable.Fixed
import io.getquill.quat.quatOf
import io.getquill.ast.Implicits._

import scala.math.BigDecimal.{double2bigDecimal, int2bigDecimal, javaBigDecimal2bigDecimal, long2bigDecimal}

case class CustomAnyValue(i: Int) extends AnyVal
case class EmbeddedValue(s: String, i: Int) extends Embedded

class QueryTest extends Spec with TestEntities {
  // remove the === matcher from scalatest so that we can test === in Context.extra
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

  // Needs to be defined outside of method otherwise Scala Bug "No TypeTag available for TestEnt" manifests.
  // See https://stackoverflow.com/a/16990806/1000455
  // TODO Is this still true of Dotty?
  case class TestEnt(ev: EmbeddedValue)
  case class TestEnt2(ev: Option[EmbeddedValue])
  case class ActionTestEntity(id: Int)

  "query" - {
    "schema" - {
      import io.getquill.quat.QuatOps.Implicits._

      "without aliases" in {
        quote(unquote(qr1)).ast mustEqual Entity("TestEntity", Nil, TestEntityQuat)
      }
      "with alias" in {
        inline def q = quote {
          querySchema[TestEntity]("SomeAlias")
        }
        quote(unquote(q)).ast mustEqual Entity.Opinionated("SomeAlias", Nil, TestEntityQuat, Fixed)
      }
      "with property alias" in {
        import io.getquill.quat.QuatOps.Implicits._

        inline def q = quote {
          querySchema[TestEntity]("SomeAlias", _.s -> "theS", _.i -> "theI")
        }
        quote(unquote(q)).ast mustEqual Entity.Opinionated(
          "SomeAlias",
          List(PropertyAlias(List("s"), "theS"), PropertyAlias(List("i"), "theI")),
          quatOf[TestEntity].productOrFail().renameAtPath(Nil, List("s" -> "theS", "i" -> "theI")),
          Fixed
        )
      }
      "with embedded property alias" in {

        inline def q = quote {
          querySchema[TestEnt]("SomeAlias", _.ev.s -> "theS", _.ev.i -> "theI")
        }
        val renamedQuat =
          quatOf[TestEnt]
            .productOrFail()
            .renameAtPath(List("ev"), List("s" -> "theS", "i" -> "theI"))
        quote(unquote(q)).ast mustEqual Entity.Opinionated("SomeAlias", List(PropertyAlias(List("ev", "s"), "theS"), PropertyAlias(List("ev", "i"), "theI")), renamedQuat, Fixed)
      }
      "with embedded option property alias" in {
        inline def q = quote {
          querySchema[TestEnt2]("SomeAlias", _.ev.map(_.s) -> "theS", _.ev.map(_.i) -> "theI")
        }
        val renamedQuat =
          quatOf[TestEnt2]
            .productOrFail()
            .renameAtPath(List("ev"), List("s" -> "theS", "i" -> "theI"))
        quote(unquote(q)).ast mustEqual Entity.Opinionated("SomeAlias", List(PropertyAlias(List("ev", "s"), "theS"), PropertyAlias(List("ev", "i"), "theI")), renamedQuat, Fixed)
      }
      "explicit `Predef.ArrowAssoc`" in {
        inline def q = quote {
          querySchema[TestEntity]("TestEntity", e => Predef.ArrowAssoc(e.s).->[String]("theS"))
        }
        val renamedQuat = TestEntityQuat.renameAtPath(Nil, List("s" -> "theS"))
        quote(unquote(q)).ast mustEqual Entity.Opinionated("TestEntity", List(PropertyAlias(List("s"), "theS")), renamedQuat, Fixed)
      }
      // TODO Is this even supported in Dotty?
      // "with property alias and unicode arrow" in {
      //   inline def q = quote {
      //     querySchema[TestEntity]("SomeAlias", _.s → "theS", _.i → "theI")
      //   }
      //   val renamedQuat = TestEntityQuat.renameAtPath(Nil, List("s" -> "theS", "i" -> "theI"))
      //   quote(unquote(q)).ast mustEqual Entity.Opinionated("SomeAlias", List(PropertyAlias(List("s"), "theS"), PropertyAlias(List("i"), "theI")), renamedQuat, Fixed)
      // }
      "with only some properties renamed" in {
        inline def q = quote {
          querySchema[TestEntity]("SomeAlias", _.s -> "theS").filter(t => t.s == "s" && t.i == 1)
        }
        val renamedQuat = TestEntityQuat.renameAtPath(Nil, List("s" -> "theS"))
        quote(unquote(q)).ast mustEqual (
          Filter(
            Entity.Opinionated("SomeAlias", List(PropertyAlias(List("s"), "theS")), renamedQuat, Fixed),
            Ident("t", renamedQuat),
            (Property(Ident("t", renamedQuat), "s") +==+ Constant.auto("s")) +&&+ (Property(Ident("t", renamedQuat), "i") +==+ Constant.auto(1))
          )
        )
      }

      case class TableData(id: Int)

      "with implicit property and generic" in {
        extension [T](q: Query[T]) {
          inline def limitQuery = quote(infix"$q LIMIT 1".as[Query[T]])
        }
        inline def q = quote { query[TableData].limitQuery }
        q.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
        quote(unquote(q)).ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.LeafProduct("id"))
      }
      // "with implicit property and generic - old style" in {
      //   implicit class LimitQuery[T](q: Query[T]) {
      //     inline def limitQuery = quote(infix"$q LIMIT 1".as[Query[T]])
      //   }
      //   inline def q = quote { query[TableData].limitQuery }
      //   q.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
      //   quote(unquote(q)).ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.LeafProduct("id"))
      // }
      // Unfortunatly the dynamic case of this is also a bug
      // "with implicit property and generic - old style - dynamic" in {
      //   implicit class LimitQuery[T](q: Query[T]) {
      //     def limitQuery = quote(infix"$q LIMIT 1".as[Query[T]])
      //   }
      //   val q = quote { query[TableData].limitQuery }
      //   println(io.getquill.util.Messages.qprint(q))
      //   q.runtimeQuotes(0).quoted.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
      // }
      "with method and generic" in {
        inline def limitQuery[T] = quote { (q: Query[T]) => infix"$q LIMIT 1".as[Query[T]] }
        inline def q = quote { limitQuery(query[TableData]) }
        q.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
        quote(unquote(q)).ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.LeafProduct("id"))
      }
      "with method and generic - typed" in {
        inline def limitQuery[T] = quote { (q: Query[T]) => infix"$q LIMIT 1".as[Query[T]] }
        inline def q = quote { limitQuery[TableData](query[TableData]) }
        q.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
        quote(unquote(q)).ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.LeafProduct("id"))
      }
    }
    "filter" in {
      inline def q = quote {
        qr1.filter(t => t.s == "s")
      }
      quote(unquote(q)).ast mustEqual Filter(
        Entity("TestEntity", Nil, TestEntityQuat),
        Ident("t", TestEntityQuat),
        BinaryOperation(Property(Ident("t", TestEntityQuat), "s"), EqualityOperator.`==`, Constant.auto("s"))
      )
    }
    "withFilter" in {
      inline def q = quote {
        qr1.withFilter(t => t.s == "s")
      }
      quote(unquote(q)).ast mustEqual Filter(
        Entity("TestEntity", Nil, TestEntityQuat),
        Ident("t"),
        BinaryOperation(Property(Ident("t"), "s"), EqualityOperator.`==`, Constant.auto("s"))
      )
    }
    "map" in {
      inline def q = quote {
        qr1.map(t => t.s)
      }
      quote(unquote(q)).ast mustEqual Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"))
    }
    "flatMap" in {
      inline def q = quote {
        qr1.flatMap(t => qr2)
      }
      quote(unquote(q)).ast mustEqual FlatMap(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
    }
    "concatMap" in {
      inline def q = quote {
        qr1.concatMap(t => t.s.split(" "))
      }
      quote(unquote(q)).ast mustEqual ConcatMap(
        Entity("TestEntity", Nil, TestEntityQuat),
        Ident("t"),
        BinaryOperation(Property(Ident("t", TestEntityQuat), "s"), StringOperator.`split`, Constant.auto(" "))
      )
    }
    "sortBy" - {
      "default ordering" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t"), "s"), AscNullsFirst)
      }
      "asc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.asc)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Asc)
      }
      "desc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.desc)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Desc)
      }
      "ascNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsFirst)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsFirst)
      }
      "descNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsFirst)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsFirst)
      }
      "ascNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsLast)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsLast)
      }
      "descNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsLast)
        }
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsLast)
      }
      "tuple" - {
        "simple" in {
          inline def q = quote {
            qr1.sortBy(t => (t.s, t.i))(Ord.desc)
          }
          quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), Desc)
        }
        "by element" in {
          inline def q = quote {
            qr1.sortBy(t => (t.s, t.i))(Ord(Ord.desc, Ord.asc))
          }
          quote(unquote(q)).ast mustEqual SortBy(
            Entity("TestEntity", Nil, TestEntityQuat),
            Ident("t"),
            Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))),
            TupleOrdering(List(Desc, Asc))
          )
        }
      }
    }
    "groupBy" in {
      inline def q = quote {
        qr1.groupBy(t => t.s)
      }
      quote(unquote(q)).ast mustEqual GroupBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t", TestEntityQuat), "s"))
    }

    "aggregation" - {
      "min" in {
        inline def q = quote {
          qr1.map(t => t.i).min
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`min`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t", TestEntityQuat), "i"))
        )
      }
      "max" in {
        inline def q = quote {
          qr1.map(t => t.i).max
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`max`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i"))
        )
      }
      "avg" in {
        inline def q = quote {
          qr1.map(t => t.i).avg
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`avg`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i"))
        )
      }
      "sum" in {
        inline def q = quote {
          qr1.map(t => t.i).sum
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`sum`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i"))
        )
      }
      "size" in {
        inline def q = quote {
          qr1.map(t => t.i).size
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`size`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i"))
        )
      }
    }

    "aggregation implicits" - {
      "min" in {
        inline def q = quote {
          qr1.map(t => t.s).min
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`min`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "s"))
        )
      }
      "max" in {
        inline def q = quote {
          qr1.map(t => t.s).max
        }
        quote(unquote(q)).ast mustEqual Aggregation(
          AggregationOperator.`max`,
          Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "s"))
        )
      }
    }
    "distinct" in {
      inline def q = quote {
        qr1.distinct
      }
      quote(unquote(q)).ast mustEqual Distinct(Entity("TestEntity", Nil, TestEntityQuat))
    }
    "nested" in {
      inline def q = quote {
        qr1.nested
      }
      quote(unquote(q)).ast mustEqual Nested(Entity("TestEntity", Nil, TestEntityQuat))
    }
    "take" in {
      inline def q = quote {
        qr1.take(10)
      }
      quote(unquote(q)).ast mustEqual Take(Entity("TestEntity", Nil, TestEntityQuat), Constant.auto(10))
    }
    "drop" in {
      inline def q = quote {
        qr1.drop(10)
      }
      quote(unquote(q)).ast mustEqual Drop(Entity("TestEntity", Nil, TestEntityQuat), Constant.auto(10))
    }
    "union" in {
      inline def q = quote {
        qr1.union(qr2)
      }
      quote(unquote(q)).ast mustEqual Union(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
    }
    "unionAll" - {
      "unionAll" in {
        inline def q = quote {
          qr1.union(qr2)
        }
        quote(unquote(q)).ast mustEqual Union(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
      }
      "++" in {
        inline def q = quote {
          qr1 ++ qr2
        }
        quote(unquote(q)).ast mustEqual UnionAll(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
      }
    }
    "join" - {

      def tree(t: JoinType) =
        Join(
          t,
          Entity("TestEntity", Nil, TestEntityQuat),
          Entity("TestEntity2", Nil, TestEntity2Quat),
          Ident("a"),
          Ident("b"),
          BinaryOperation(Property(Ident("a"), "s"), EqualityOperator.`==`, Property(Ident("b"), "s"))
        )

      "inner join" in {
        inline def q = quote {
          qr1.join(qr2).on((a, b) => a.s == b.s)
        }
        quote(unquote(q)).ast mustEqual tree(InnerJoin)
      }
      "left join" in {
        inline def q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
        }
        quote(unquote(q)).ast mustEqual tree(LeftJoin)
      }
      "right join" in {
        inline def q = quote {
          qr1.rightJoin(qr2).on((a, b) => a.s == b.s)
        }
        quote(unquote(q)).ast mustEqual tree(RightJoin)
      }
      "full join" in {
        inline def q = quote {
          qr1.fullJoin(qr2).on((a, b) => a.s == b.s)
        }
        quote(unquote(q)).ast mustEqual tree(FullJoin)
      }

      "fails if not followed by 'on'" in {
        """
          quote {
            qr1.fullJoin(qr2)
          }
        """ mustNot compile
      }
    }
  }
}
