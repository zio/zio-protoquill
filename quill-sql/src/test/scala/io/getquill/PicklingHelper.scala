package io.getquill

import io.getquill.ast.Ast
import io.getquill.quat.Quat
import io.getquill.parser.BooSerializer

object PicklingHelper {
  def repickle(ast: Ast) =
    BooSerializer.Ast.deserialize(BooSerializer.Ast.serialize(ast))

  def repickle(quat: Quat) =
    BooSerializer.Quat.deserialize(BooSerializer.Quat.serialize(quat))
}
