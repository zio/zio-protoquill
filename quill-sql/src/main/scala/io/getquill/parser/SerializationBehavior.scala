package io.getquill.parser

sealed trait SerializationBehavior
object SerializationBehavior {
  sealed trait SkipSerialize extends SerializationBehavior
  case object SkipSerialize extends SkipSerialize
  sealed trait Default extends SerializationBehavior
  case object Default extends Default
}

trait DoSerialize {
  type BehaviorType <: SerializationBehavior
}
