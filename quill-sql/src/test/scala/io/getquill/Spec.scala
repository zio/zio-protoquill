package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import io.getquill.Quoted
import io.getquill.EagerPlanter
import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.NamingStrategy
import io.getquill.idiom.Idiom
import io.getquill.Query
import io.getquill._
import io.getquill.Dsl._

abstract class Spec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  
  extension [T](m: MirrorContext[_, _]#ActionMirror)
    def triple = 
      (
        m.string, 
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row => 
            r.data.toList.map(_._2)
        }, 
        m.executionType
      )

  extension [T, D <: Idiom, N <: NamingStrategy](ctx: MirrorContext[D, N])
    inline def pull(inline q: Query[T]) =
      val r = ctx.run(q)
      (
        r.prepareRow match {
          case r: io.getquill.context.mirror.Row => 
            r.data.toList.map(_._2)
        }, 
        r.executionType
      )

  extension [T, PrepareRow](q: Quoted[T])
    def encodeEagerLifts(row: PrepareRow) =
      q.lifts.zipWithIndex.collect {
        case (ep: EagerPlanter[String, PrepareRow], idx) => ep.encoder(idx, ep.value, row)
      }
  

  object ShortAst {
    object Id {
      def apply(str: String, quat: Quat) = Ident(str, quat)
      def unapply(id: Ident) = Some(id.name)
    }
    object Ent {
      def apply(name: String, quat: Quat.Product) = Entity(name, Nil, quat)
      def unapply(entity: Entity) = Some(entity.name)
    }
    object `(+)` {
      def apply(a: Ast, b: Ast) = BinaryOperation(a, StringOperator.+, b)
      def unapply(ast: Ast) = ast match
        case BinaryOperation(a, StringOperator.+, b) => Some(a, b)
        case _ => None
    }
  }

  def await[T](f: Future[T]): T = Await.result(f, Duration.Inf)
}
