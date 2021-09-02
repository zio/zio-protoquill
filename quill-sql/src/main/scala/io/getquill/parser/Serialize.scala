package io.getquill.parser

import io.getquill.util.ProtoMessages

enum SerializeQuat:
  case All
  case ByFieldCount(maxFields: Int) extends SerializeQuat
  case None

object SerializeQuat:
  def global =
    if (ProtoMessages.maxQuatFields == 0) SerializeQuat.All
    else if (ProtoMessages.maxQuatFields < 0) SerializeQuat.None
    else SerializeQuat.ByFieldCount(ProtoMessages.maxQuatFields)

enum SerializeAst:
  case All
  case None

object SerializeAst:
  def global: SerializeAst =
    if (ProtoMessages.serializeAst) SerializeAst.All else SerializeAst.None
