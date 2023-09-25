package io.getquill.context

import io.getquill.ReturnAction

sealed trait RunnerSummoningBehavior
object RunnerSummoningBehavior {
  sealed trait Implicit extends RunnerSummoningBehavior
  object Implicit       extends Implicit
  sealed trait Member   extends RunnerSummoningBehavior
  object Member         extends Member
}

sealed trait Extraction[-ResultRow, -Session, +T]:
  /**
   * Require an effect to be be simple and retrieve it. Effectful at
   * compile-time since it can fail compilation
   */
  def requireSimple() =
    this match
      case ext: Extraction.Simple[_, _, _] => ext
      case _                               => throw new IllegalArgumentException("Extractor required")

  /**
   * Require an effect to be be returning and retrieve it. Effectful at
   * compile-time since it can fail compilation
   */
  def requireReturning() =
    this match
      case ext: Extraction.Returning[_, _, _] => ext
      case _                                  => throw new IllegalArgumentException("Returning Extractor required")

object Extraction:
  case class Simple[ResultRow, Session, T](extract: (ResultRow, Session) => T) extends Extraction[ResultRow, Session, T]
  case class Returning[ResultRow, Session, T](extract: (ResultRow, Session) => T, returningBehavior: ReturnAction)
      extends Extraction[ResultRow, Session, T]
  case object None extends Extraction[Any, Any, Nothing]
