package io.getquill.parser

import io.getquill.util.ProtoMessages
import io.getquill.util.Format

enum SerializeQuat {
  case All
  case ByFieldCount(maxFields: Int) extends SerializeQuat
  case None
}

object SerializeQuat {
  object Lifter {
    import scala.quoted._
    given FromExpr[SerializeQuat] with {
      def unapply(expr: Expr[SerializeQuat])(using Quotes) = {
        import quotes.reflect._
        expr match {
          case '{ SerializeQuat.All }                        => Some(SerializeQuat.All)
          case '{ SerializeQuat.ByFieldCount(${ Expr(i) }) } => Some(SerializeQuat.ByFieldCount(i))
          case '{ SerializeQuat.None }                       => Some(SerializeQuat.None)
          case _                                             => Option.empty[SerializeQuat]
        }
      }
    }
    def apply(expr: Expr[SerializeQuat])(using Quotes) = {
      import quotes.reflect._
      expr.asTerm.underlyingArgument.asExprOf[SerializeQuat] match {
        case Expr(serializeQuat) => serializeQuat
        case other => report.throwError(
            s"""|Found an implicit instrument to Serialize Quats but could not read it from expression: ${Format.Expr(other)}.
              |Make sure that the SerializeQuat implicit is defined as an inline-given (or implicit inline def) for example:
              |inline given SerializeQuat = SerializeQuat.All
              |val q = quote { myQuery } // will use the above given
           """
          )
      }
    }
  }

  def global =
    if (ProtoMessages.maxQuatFields == 0) SerializeQuat.All
    else if (ProtoMessages.maxQuatFields < 0) SerializeQuat.None
    else SerializeQuat.ByFieldCount(ProtoMessages.maxQuatFields)

} // end SerializeQuat

enum SerializeAst {
  case All
  case None
}

object SerializeAst {
  def global: SerializeAst =
    if (ProtoMessages.serializeAst) SerializeAst.All else SerializeAst.None

  object Lifter {
    import scala.quoted._
    given FromExpr[SerializeAst] with {
      def unapply(expr: Expr[SerializeAst])(using Quotes) = {
        import quotes.reflect._
        expr match {
          case '{ SerializeAst.All }  => Some(SerializeAst.All)
          case '{ SerializeAst.None } => Some(SerializeAst.None)
          case _                      => Option.empty[SerializeAst]
        }
      }
    }
    def apply(expr: Expr[SerializeAst])(using Quotes) = {
      import quotes.reflect._
      expr.asTerm.underlyingArgument.asExprOf[SerializeAst] match {
        case Expr(serializeAst) => serializeAst
        case other => report.throwError(
            s"""|Found an implicit instrument to Serialize Asts but could not read it from expression: ${Format.Expr(other)}.
              |Make sure that the SerializeAst implicit is defined as an inline-given (or implicit inline def) for example:
              |inline given SerializeAst = SerializeAst.All
              |val q = quote { myQuery } // will use the above given
            """
          )
      }
    }
  }

} // end SerializeAst
