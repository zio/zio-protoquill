package io.getquill

import io.getquill.ast.{Ast, BinaryOperation, Entity, Function, Ident, StringOperator}
import io.getquill.context.mirror.{MirrorSession, Row}
import io.getquill.idiom.Idiom
import io.getquill.quat.Quat

trait MirrorSpec extends Spec with MirrorContext.Codec {
  type Session = MirrorSession
  type PrepareRow = Row
  type ResultRow = Row

  extension (m: MirrorContextBase[_, _]#BatchActionReturningMirror[_]) {
    def triple = {
      if (m.groups.length != 1) fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, returnAction, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data.map(data => deIndexify(data._2))
          }
        },
        m.info.executionType
      )
    }
  }

  extension (m: MirrorContextBase[_, _]#BatchActionReturningMirror[_]) {
    def tripleBatchMulti =
      m.groups.map { (queryString, returnAction, prepares) =>
        (
          queryString,
          prepares.map { prep =>
            // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
            prep match {
              case r: io.getquill.context.mirror.Row =>
                r.data.map(data => deIndexify(data._2))
            }
          },
          m.info.executionType
        )
      }
  }

  private def deIndexify(value: Any): Any =
    value match {
      case Some((Row.TupleIndex(a) -> b)) => Some(deIndexify(b))
      case list: Seq[Any] => list.map(deIndexify(_))
      case Row.TupleIndex(a) -> b => b
      case other => other
    }

  extension (m: MirrorContextBase[_, _]#BatchActionMirror) {
    def triple = {
      if (m.groups.length != 1) fail(s"Expected all batch groups per design to only have one root element but has multiple ${m.groups}")
      val (queryString, prepares) = m.groups(0)
      (
        queryString,
        prepares.map { prep =>
          // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
          prep match {
            case r: io.getquill.context.mirror.Row =>
              r.data.map(data => deIndexify(data._2))
          }
        },
        m.info.executionType
      )
    }
  }

  extension (m: MirrorContextBase[_, _]#BatchActionMirror) {
    def tripleBatchMulti =
      m.groups.map { (queryString, prepares) =>
        (
          queryString,
          prepares.map { prep =>
            // being explicit here about the fact that this is done per prepare element i.e. all of them are supposed to be Row instances
            prep match {
              case r: io.getquill.context.mirror.Row =>
                r.data.map(data => deIndexify(data._2))
            }
          },
          m.info.executionType
        )
      }
  }

  extension (m: MirrorContextBase[_, _]#ActionMirror) {
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.map(data => deIndexify(data._2))
        },
        m.info.executionType
      )
  }

  extension (m: MirrorContextBase[_, _]#ActionReturningMirror[_, _]) {
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.map(data => deIndexify(data._2))
        },
        m.info.executionType
      )
  }

  extension [T](m: MirrorContextBase[_, _]#QueryMirror[_]) {
    def triple =
      (
        m.string,
        m.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.map(data => deIndexify(data._2))
        },
        m.info.executionType
      )
  }

  extension [T, D <: Idiom, N <: NamingStrategy](ctx: MirrorContextBase[D, N]) {
    inline def pull(inline q: Query[T]) = {
      val r = ctx.run(q)
      (
        r.prepareRow match {
          case r: io.getquill.context.mirror.Row =>
            r.data.toList.map(_._2)
        },
        r.info.executionType
      )
    }
  }

  extension [T, PrepareRow, Session](q: Quoted[T]) {
    def encodeEagerLifts(row: PrepareRow, session: Session) =
      q.lifts.zipWithIndex.collect {
        case (ep: EagerPlanter[String, PrepareRow, Session], idx) => ep.encoder(idx, ep.value, row, session)
      }
  }

  extension (ast: Ast) {
    def asFunction = ast.asInstanceOf[Function]
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

      def unapply(ast: Ast) =
        ast match {
          case BinaryOperation(a, StringOperator.+, b) => Some(a, b)
          case _ => None
        }
    }
  }
}