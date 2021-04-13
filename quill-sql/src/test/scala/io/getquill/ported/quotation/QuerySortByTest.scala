package io.getquill.ported.quotation

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Dsl._
import io.getquill._
import io.getquill._
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import org.scalatest._
import io.getquill.quat.Quat

class QuerySortByTest extends Spec with Inside with TestEntities {
  // val ctx = new MirrorContext(MirrorIdiom, Literal)
  // import ctx._

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
        quote(unquote(q)).ast mustEqual SortBy(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), Tuple(List(Property(Ident("t"), "s"), Property(Ident("t"), "i"))), TupleOrdering(List(Desc, Asc)))
      }
    }
  }
}