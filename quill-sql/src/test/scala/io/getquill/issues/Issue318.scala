package io.getquill.issues

import io.getquill.*

case class Custom(name: String)



class Issue318 extends Spec {
  "reproduce" in {
    val nameFilter = Seq("a", "b")

    val ctx = new SqlMirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    val query = dynamicQuery[Custom].filter(v =>
      nameFilter.map(name => quote(sql"${v.name} == ${lift(name)}".asCondition))
      .reduce((l,r) => quote(l || r))
    )
    val sql = ctx.translate(query)
    assert(sql.contains("'a'"))
    assert(sql.contains("'b'"))
  }
}
