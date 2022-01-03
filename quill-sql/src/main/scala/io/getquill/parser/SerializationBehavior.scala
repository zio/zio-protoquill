package io.getquill.parser

sealed trait SerializationBehavior
object SerializationBehavior:
  sealed trait Serialize extends SerializationBehavior
  case object Serialize extends Serialize
  sealed trait Skip extends SerializationBehavior
  case object Skip extends Skip

trait DoSerialize:
  type BehaviorType <: SerializationBehavior
