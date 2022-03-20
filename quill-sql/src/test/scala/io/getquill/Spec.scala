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

abstract class Spec extends AnyFreeSpec with Matchers with BeforeAndAfterAll {
  val QV = Quat.Value
  val QEP = Quat.Product.empty
  def QP(fields: String*) = Quat.LeafProduct(fields: _*)

  extension (m: MirrorContext[_, _]#BatchActionReturningMirror[_])
    def triple =
      if (m.groups.length != 1) fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, returnAction, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data.toList.map(_._2)
          }
        },
        m.info.executionType
      )

  extension (m: MirrorContext[_, _]#BatchActionMirror)
    def triple =
      if (m.groups.length != 1) fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data.toList.map(_._2)
          }
        },
        m.info.executionType
      )

  extension (m: MirrorContext[_, _]#ActionMirror)
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.toList.map(_._2)
        },
        m.info.executionType
      )

  extension (m: MirrorContext[_, _]#ActionReturningMirror[_])
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.toList.map(_._2)
        },
        m.info.executionType
      )

  extension [T](m: MirrorContext[_, _]#QueryMirror[_])
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.toList.map(_._2)
        },
        m.info.executionType
      )

  extension [T, D <: Idiom, N <: NamingStrategy](ctx: MirrorContext[D, N])
    inline def pull(inline q: Query[T]) =
      val r = ctx.run(q)
      (
        r.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.toList.map(_._2)
        },
        r.info.executionType
      )

  extension [T, PrepareRow, Session](q: Quoted[T])
    def encodeEagerLifts(row: PrepareRow, session: Session) =
      q.lifts.zipWithIndex.collect {
        case (ep: EagerPlanter[String, PrepareRow, Session], idx) => ep.encoder(idx, ep.value, row, session)
      }

  extension (ast: Ast)
    def asFunction = ast.asInstanceOf[Function]

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

  extension (quat: Quat) {
    def productOrFail() =
      quat match {
        case p: Quat.Product => p
        case _               => throw new IllegalArgumentException(s"The quat ${quat} is expected to be a product but is not")
      }
  }

  case class NameChangeIdent(nameChange: PartialFunction[String, String]) extends StatelessTransformer:
    override def applyIdent(id: Ident) = id.copy(name = nameChange.lift(id.name).getOrElse(id.name))
}
