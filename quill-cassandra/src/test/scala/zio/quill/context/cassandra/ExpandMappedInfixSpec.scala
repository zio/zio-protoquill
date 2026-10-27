package zio.quill.context.cassandra

import zio.quill._
import zio.quill.ast.Ast

class ExpandMappedInfixSpec extends Spec {

  import mirrorContext._

  "removes identity map" in {
    inline def i = quote {
      sql"test".as[Query[Int]]
    }
    inline def q = quote {
      i.map(x => x)
    }
    ExpandMappedInfixCassandra(q.ast: Ast) mustEqual i.ast
  }

  "expands mapped infix wrapping single query" in {
    inline def q = quote {
      sql"$qr1 ALLOW FILTERING".as[Query[TestEntity]].map(t => t.i)
    }
    inline def n = quote {
      sql"${qr1.map(t => t.i)} ALLOW FILTERING".as[Query[TestEntity]]
    }
    ExpandMappedInfixCassandra(q.ast: Ast) mustEqual n.ast
  }

}
