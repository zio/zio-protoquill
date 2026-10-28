package zio.quill

import zio.quill.ast.Ast
import zio.quill.quat.Quat
import zio.quill.parser.BooSerializer

object PicklingHelper {
  def repickle(ast: Ast) =
    BooSerializer.Ast.deserialize(BooSerializer.Ast.serialize(ast))

  def repickle(quat: Quat) =
    BooSerializer.Quat.deserialize(BooSerializer.Quat.serialize(quat))
}
