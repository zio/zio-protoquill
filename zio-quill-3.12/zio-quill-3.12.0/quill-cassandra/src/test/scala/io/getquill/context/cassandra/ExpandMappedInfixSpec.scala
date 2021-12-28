package io.getquill.context.cassandra

import io.getquill._
import io.getquill.ast.Ast

class ExpandMappedInfixSpec extends Spec {

  import mirrorContext._

  "removes identity map" in {
    inline def i = quote {
      infix"test".as[Query[Int]]
    }
    inline def q = quote {
      i.map(x => x)
    }
    ExpandMappedInfix(q.ast: Ast) mustEqual i.ast
  }

  "expands mapped infix wrapping single query" in {
    inline def q = quote {
      infix"$qr1 ALLOW FILTERING".as[Query[TestEntity]].map(t => t.i)
    }
    inline def n = quote {
      infix"${qr1.map(t => t.i)} ALLOW FILTERING".as[Query[TestEntity]]
    }
    ExpandMappedInfix(q.ast: Ast) mustEqual n.ast
  }

}
