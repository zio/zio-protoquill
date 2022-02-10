package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.ast.{Query => AQuery, _}
import io.getquill._
import org.scalatest._
import io.getquill.quat.Quat
import io.getquill.Spec
import io.getquill.PicklingHelper._

class QuerySortByTest extends Spec with Inside with TestEntities {
  // val ctx = new MirrorContext(MirrorIdiom, Literal)
  // import ctx._

  "sortBy" - {
    "default ordering" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t", TestEntityQuat), Property(Ident("t"), "s"), AscNullsFirst)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "asc" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.asc)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Asc)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "desc" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.desc)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), Desc)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "ascNullsFirst" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.ascNullsFirst)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsFirst)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "descNullsFirst" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.descNullsFirst)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsFirst)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "ascNullsLast" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.ascNullsLast)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), AscNullsLast)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "descNullsLast" in {
      inline def q = quote {
        qr1.sortBy(t => t.s)(Ord.descNullsLast)
      }
      val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Property(Ident("t"), "s"), DescNullsLast)
      quote(unquote(q)).ast mustEqual s
      repickle(s) mustEqual s
    }
    "tuple" - {
      "simple" in {
        inline def q = quote {
          qr1.sortBy(t => (t.s, t.i))(Ord.desc)
        }
        val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), Desc)
        quote(unquote(q)).ast mustEqual s
        repickle(s) mustEqual s
      }
      "by element" in {
        inline def q = quote {
          qr1.sortBy(t => (t.s, t.i))(Ord(Ord.desc, Ord.asc))
        }
        val s = SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), TupleOrdering(List(Desc, Asc)))
        quote(unquote(q)).ast mustEqual s
        repickle(s) mustEqual s
      }
    }
  }
}