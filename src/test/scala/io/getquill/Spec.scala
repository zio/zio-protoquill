package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import io.getquill.quoter.Quoted
import io.getquill.quoter.EagerPlanter
import io.getquill.ast._

abstract class Spec extends AnyFreeSpec with Matchers /* with BeforeAndAfterAll */ {

  extension [T, PrepareRow](q: Quoted[T]) {
    def encodeEagerLifts(row: PrepareRow) =
      q.lifts.zipWithIndex.collect {
        case (ep: EagerPlanter[String, PrepareRow], idx) => ep.encoder(idx, ep.value, row)
      }
  }

  object ShortAst {
    object Id {
      def apply(str: String) = Ident(str)
      def unapply(id: Ident) = Some(id.name)
    }
    object Ent {
      def apply(name: String) = Entity(name, Nil)
      def unapply(entity: Entity) = Some(entity.name)
    }
    object `(+)` {
      def apply(a: Ast, b: Ast) = BinaryOperation(a, StringOperator.+, b)
      def unapply(ast: Ast) = ast match
        case BinaryOperation(a, StringOperator.+, b) => Some(a, b)
        case _ => None
    }
  }

  // // =============== Not technically needed but otherwise vscode+dotty gives too many false errors ===============
  // implicit def convertToFreeSpecStringWrapperNoImplicit(s: String): FreeSpecStringWrapper = 
  //   new FreeSpecStringWrapper(s, implicitly[org.scalactic.source.Position])
  // // remove this one from the implicit
  // override def convertToAnyMustWrapper[T](value: T)(implicit pos: org.scalactic.source.Position, pret: org.scalactic.Prettifier): AnyMustWrapper[T] =
  //   new AnyMustWrapper(value, pos, pret)
  // // remove this one from the implicit
  // override def convertToStringMustWrapper(value: String)(implicit pos: org.scalactic.source.Position, pret: org.scalactic.Prettifier): StringMustWrapper =
  //   new StringMustWrapper(value, pos, pret)
  // // add them back without implicits
  // implicit def convertToAnyMustWrapperNoImplicit[T](value: T): AnyMustWrapper[T] =
  //   new AnyMustWrapper(value, implicitly[org.scalactic.source.Position], implicitly[org.scalactic.Prettifier])
  // implicit def convertToStringMustWrapperNoImplicit(value: String): StringMustWrapper =
  //   new StringMustWrapper(value, implicitly[org.scalactic.source.Position], implicitly[org.scalactic.Prettifier])
  // // ================================================================================================

  def await[T](f: Future[T]): T = Await.result(f, Duration.Inf)
}
