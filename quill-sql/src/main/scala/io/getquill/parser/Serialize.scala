package io.getquill.parser

import io.getquill.util.ProtoMessages

enum SerializeQuat:
  case All
  case None

object SerializeQuat:
  def global(numQuatFields: Int) =
    if (numQuatFields > ProtoMessages.maxQuatFields) SerializeQuat.All else SerializeQuat.None

enum SerializeAst:
  case All
  case None

object SerializeAst:
  def global: SerializeAst =
    if (ProtoMessages.serializeAst) SerializeAst.All else SerializeAst.None