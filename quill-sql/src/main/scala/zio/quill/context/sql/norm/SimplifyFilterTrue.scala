package zio.quill.context.sql.norm

import zio.quill.ast.StatelessTransformer
import zio.quill.quat.Quat.BooleanValue
import zio.quill.ast._

object SimplifyFilterTrue extends StatelessTransformer {
  val trueVal = Constant(true, BooleanValue)

  override def apply(ast: Ast): Ast =
    ast match {
      case Filter(q: Query, id, `trueVal`) => super.apply(q)
      case _                               => super.apply(ast)
    }
}
