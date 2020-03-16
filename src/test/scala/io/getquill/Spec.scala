package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.MustMatchers

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration

abstract class Spec extends AnyFreeSpec with MustMatchers /* with BeforeAndAfterAll */ {

  // =============== Not technically needed but otherwise vscode+dotty gives too many false errors ===============
  implicit def convertToFreeSpecStringWrapperNoImplicit(s: String): FreeSpecStringWrapper = 
    new FreeSpecStringWrapper(s, implicitly[org.scalactic.source.Position])
  // remove this one from the implicit
  override def convertToAnyMustWrapper[T](value: T)(implicit pos: org.scalactic.source.Position, pret: org.scalactic.Prettifier): AnyMustWrapper[T] =
    new AnyMustWrapper(value, pos, pret)
  // remove this one from the implicit
  override def convertToStringMustWrapper(value: String)(implicit pos: org.scalactic.source.Position, pret: org.scalactic.Prettifier): StringMustWrapper =
    new StringMustWrapper(value, pos, pret)
  // add them back without implicits
  implicit def convertToAnyMustWrapperNoImplicit[T](value: T): AnyMustWrapper[T] =
    new AnyMustWrapper(value, implicitly[org.scalactic.source.Position], implicitly[org.scalactic.Prettifier])
  implicit def convertToStringMustWrapperNoImplicit(value: String): StringMustWrapper =
    new StringMustWrapper(value, implicitly[org.scalactic.source.Position], implicitly[org.scalactic.Prettifier])
  // ================================================================================================

  def await[T](f: Future[T]): T = Await.result(f, Duration.Inf)
}
