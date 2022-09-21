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
import io.getquill.PicklingHelper._

import scala.math.BigDecimal.{ double2bigDecimal, int2bigDecimal, javaBigDecimal2bigDecimal, long2bigDecimal }

case class CustomAnyValue(i: Int) extends AnyVal
case class EmbeddedValue(s: String, i: Int) extends Embedded

class QueryTest extends Spec with TestEntities {

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
        val e = Entity("TestEntity", Nil, TestEntityQuat)
        quote(unquote(qr1)).ast mustEqual e
        repickle(e) mustEqual e
      }
      "with alias" in {
        inline def q = quote {
          querySchema[TestEntity]("SomeAlias")
        }
        val e = Entity.Opinionated("SomeAlias", Nil, TestEntityQuat, Fixed)
        quote(unquote(q)).ast mustEqual e
        repickle(e) mustEqual e
      }
      "with property alias" in {
        import io.getquill.quat.QuatOps.Implicits._

        inline def q = quote {
          querySchema[TestEntity]("SomeAlias", _.s -> "theS", _.i -> "theI")
        }
        val e =
          Entity.Opinionated(
            "SomeAlias",
            List(PropertyAlias(List("s"), "theS"), PropertyAlias(List("i"), "theI")),
            quatOf[TestEntity].productOrFail().renameAtPath(Nil, List("s" -> "theS", "i" -> "theI")),
            Fixed
          )
        quote(unquote(q)).ast mustEqual e
        repickle(e) mustEqual e
      }
      "with embedded property alias" in {

        inline def q = quote {
          querySchema[TestEnt]("SomeAlias", _.ev.s -> "theS", _.ev.i -> "theI")
        }
        val renamedQuat =
          quatOf[TestEnt]
            .productOrFail()
            .renameAtPath(List("ev"), List("s" -> "theS", "i" -> "theI"))
        val e = Entity.Opinionated("SomeAlias", List(PropertyAlias(List("ev", "s"), "theS"), PropertyAlias(List("ev", "i"), "theI")), renamedQuat, Fixed)
        quote(unquote(q)).ast mustEqual e
        repickle(e) mustEqual e
      }
      "with embedded option property alias" in {
        inline def q = quote {
          querySchema[TestEnt2]("SomeAlias", _.ev.map(_.s) -> "theS", _.ev.map(_.i) -> "theI")
        }
        val renamedQuat =
          quatOf[TestEnt2]
            .productOrFail()
            .renameAtPath(List("ev"), List("s" -> "theS", "i" -> "theI"))
        val e = Entity.Opinionated("SomeAlias", List(PropertyAlias(List("ev", "s"), "theS"), PropertyAlias(List("ev", "i"), "theI")), renamedQuat, Fixed)
        quote(unquote(q)).ast mustEqual e
        repickle(e) mustEqual e
      }
      "explicit `Predef.ArrowAssoc`" in {
        inline def q = quote {
          querySchema[TestEntity]("TestEntity", e => Predef.ArrowAssoc(e.s).->[String]("theS"))
        }
        val renamedQuat = TestEntityQuat.renameAtPath(Nil, List("s" -> "theS"))
        val e = Entity.Opinionated("TestEntity", List(PropertyAlias(List("s"), "theS")), renamedQuat, Fixed)
        quote(unquote(q)).ast mustEqual e
        repickle(e) mustEqual e
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
        val f =
          Filter(Entity.Opinionated("SomeAlias", List(PropertyAlias(List("s"), "theS")), renamedQuat, Fixed), Ident("t", renamedQuat),
            (Property(Ident("t", renamedQuat), "s") +==+ Constant.auto("s")) +&&+ (Property(Ident("t", renamedQuat), "i") +==+ Constant.auto(1))
          )
        quote(unquote(q)).ast mustEqual f
        repickle(f) mustEqual f
      }

      case class TableData(id: Int)

      "with implicit property and generic" in {
        extension [T](q: Query[T]) {
          inline def limitQuery = quote(sql"$q LIMIT 1".as[Query[T]])
        }
        inline def q = quote { query[TableData].limitQuery }
        val parseTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.Generic)
        val evalTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.LeafProduct("id"))
        q.ast mustEqual parseTime
        quote(unquote(q)).ast mustEqual evalTime

        repickle(parseTime) mustEqual parseTime
        repickle(evalTime) mustEqual evalTime
      }
      // "with implicit property and generic - old style" in {
      //   implicit class LimitQuery[T](q: Query[T]) {
      //     inline def limitQuery = quote(sql"$q LIMIT 1".as[Query[T]])
      //   }
      //   inline def q = quote { query[TableData].limitQuery }
      //   q.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
      //   quote(unquote(q)).ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.LeafProduct("id"))
      // }
      // Unfortunately the dynamic case of this is also a bug
      // "with implicit property and generic - old style - dynamic" in {
      //   implicit class LimitQuery[T](q: Query[T]) {
      //     def limitQuery = quote(sql"$q LIMIT 1".as[Query[T]])
      //   }
      //   val q = quote { query[TableData].limitQuery }
      //   println(io.getquill.util.Messages.qprint(q))
      //   q.runtimeQuotes(0).quoted.ast mustEqual Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, Quat.Generic)
      // }
      "with method and generic" in {
        inline def limitQuery[T] = quote { (q: Query[T]) => sql"$q LIMIT 1".as[Query[T]] }
        inline def q = quote { limitQuery(query[TableData]) }
        val parseTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.Generic)
        val evalTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.LeafProduct("id"))
        q.ast mustEqual parseTime
        quote(unquote(q)).ast mustEqual evalTime

        repickle(parseTime) mustEqual parseTime
        repickle(evalTime) mustEqual evalTime
      }
      "with method and generic - typed" in {
        inline def limitQuery[T] = quote { (q: Query[T]) => sql"$q LIMIT 1".as[Query[T]] }
        inline def q = quote { limitQuery[TableData](query[TableData]) }
        val parseTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.Generic)
        val evalTime = Infix(List("", " LIMIT 1"), List(Entity("TableData", List(), Quat.LeafProduct("id"))), false, false, Quat.LeafProduct("id"))
        q.ast mustEqual parseTime
        quote(unquote(q)).ast mustEqual evalTime


      }
    }
    "filter" in {
      inline def q = quote {
        qr1.filter(t => t.s == "s")
      }
      val qry = Filter(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), BinaryOperation(Property(Ident("t", TestEntityQuat), "s"), EqualityOperator.`_==`, Constant.auto("s")))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "withFilter" in {
      inline def q = quote {
        qr1.withFilter(t => t.s == "s")
      }
      val qry = Filter(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), BinaryOperation(Property(Ident("t"), "s"), EqualityOperator.`_==`, Constant.auto("s")))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "map" in {
      inline def q = quote {
        qr1.map(t => t.s)
      }
      val qry = Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "flatMap" in {
      inline def q = quote {
        qr1.flatMap(t => qr2)
      }
      val qry = FlatMap(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "concatMap" in {
      inline def q = quote {
        qr1.concatMap(t => t.s.split(" "))
      }
      val qry = ConcatMap(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), BinaryOperation(Property(Ident("t", TestEntityQuat), "s"), StringOperator.`split`, Constant.auto(" ")))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "sortBy" - {
      "default ordering" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t"), "s"), AscNullsFirst)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "asc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.asc)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Asc)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "desc" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.desc)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Desc)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "ascNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsFirst)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsFirst)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "descNullsFirst" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsFirst)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsFirst)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "ascNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.ascNullsLast)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsLast)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "descNullsLast" in {
        inline def q = quote {
          qr1.sortBy(t => t.s)(Ord.descNullsLast)
        }
        val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsLast)
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "tuple" - {
        "simple" in {
          inline def q = quote {
            qr1.sortBy(t => (t.s, t.i))(Ord.desc)
          }
          val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), Desc)
          quote(unquote(q)).ast mustEqual qry
          repickle(qry) mustEqual qry
        }
        "by element" in {
          inline def q = quote {
            qr1.sortBy(t => (t.s, t.i))(Ord(Ord.desc, Ord.asc))
          }
          val qry = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), TupleOrdering(List(Desc, Asc)))
          quote(unquote(q)).ast mustEqual qry
          repickle(qry) mustEqual qry
        }
      }
    }
    "groupBy" in {
      inline def q = quote {
        qr1.groupBy(t => t.s)
      }
      val qry = GroupBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t", TestEntityQuat), "s"))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }

    "aggregation" - {
      "min" in {
        inline def q = quote {
          qr1.map(t => t.i).min
        }
        val qry = Aggregation(AggregationOperator.`min`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t", TestEntityQuat), "i")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "max" in {
        inline def q = quote {
          qr1.map(t => t.i).max
        }
        val qry = Aggregation(AggregationOperator.`max`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "avg" in {
        inline def q = quote {
          qr1.map(t => t.i).avg
        }
        val qry = Aggregation(AggregationOperator.`avg`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "sum" in {
        inline def q = quote {
          qr1.map(t => t.i).sum
        }
        val qry = Aggregation(AggregationOperator.`sum`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "size" in {
        inline def q = quote {
          qr1.map(t => t.i).size
        }
        val qry = Aggregation(AggregationOperator.`size`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "i")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
    }

    "aggregation implicits" - {
      "min" in {
        inline def q = quote {
          qr1.map(t => t.s).min
        }
        val qry = Aggregation(AggregationOperator.`min`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "s")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "max" in {
        inline def q = quote {
          qr1.map(t => t.s).max
        }
        val qry = Aggregation(AggregationOperator.`max`, Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t", TestEntityQuat), "s")))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
    }
    "distinct" in {
      inline def q = quote {
        qr1.distinct
      }
      val qry = Distinct(Entity("TestEntity", Nil, TestEntityQuat))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "nested" in {
      inline def q = quote {
        qr1.nested
      }
      val qry = Nested(Entity("TestEntity", Nil, TestEntityQuat))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "take" in {
      inline def q = quote {
        qr1.take(10)
      }
      val qry = Take(Entity("TestEntity", Nil, TestEntityQuat), Constant.auto(10))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "drop" in {
      inline def q = quote {
        qr1.drop(10)
      }
      val qry = Drop(Entity("TestEntity", Nil, TestEntityQuat), Constant.auto(10))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "union" in {
      inline def q = quote {
        qr1.union(qr2)
      }
      val qry = Union(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
      quote(unquote(q)).ast mustEqual qry
      repickle(qry) mustEqual qry
    }
    "unionAll" - {
      "unionAll" in {
        inline def q = quote {
          qr1.union(qr2)
        }
        val qry = Union(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
      "++" in {
        inline def q = quote {
          qr1 ++ qr2
        }
        val qry = UnionAll(Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat))
        quote(unquote(q)).ast mustEqual qry
        repickle(qry) mustEqual qry
      }
    }
    "join" - {

      def tree(t: JoinType) =
        Join(t, Entity("TestEntity", Nil, TestEntityQuat), Entity("TestEntity2", Nil, TestEntity2Quat), Ident("a"), Ident("b"), BinaryOperation(Property(Ident("a"), "s"), EqualityOperator.`_==`, Property(Ident("b"), "s")))

      "inner join" in {
        inline def q = quote {
          qr1.join(qr2).on((a, b) => a.s == b.s)
        }
        val t = tree(InnerJoin)
        quote(unquote(q)).ast mustEqual t
        repickle(t) mustEqual t
      }
      "left join" in {
        inline def q = quote {
          qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
        }
        val t = tree(LeftJoin)
        quote(unquote(q)).ast mustEqual t
        repickle(t) mustEqual t
      }
      "right join" in {
        inline def q = quote {
          qr1.rightJoin(qr2).on((a, b) => a.s == b.s)
        }
        val t = tree(RightJoin)
        quote(unquote(q)).ast mustEqual t
        repickle(t) mustEqual t
      }
      "full join" in {
        inline def q = quote {
          qr1.fullJoin(qr2).on((a, b) => a.s == b.s)
        }
        val t = tree(FullJoin)
        quote(unquote(q)).ast mustEqual t
        repickle(t) mustEqual t
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