package io.getquill.context.sql.norm

import io.getquill.ast.StatelessTransformer
import io.getquill.quat.Quat.BooleanValue
import io.getquill.ast._

object SimplifyFilterTrue extends StatelessTransformer {
  val trueVal = Constant(true, BooleanValue)

  override def apply(ast: Ast): Ast =
    ast match {
      case Filter(q: Query, id, `trueVal`) => super.apply(q)
      case _ => super.apply(ast)
    }
}
