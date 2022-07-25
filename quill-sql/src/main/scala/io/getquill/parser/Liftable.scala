package io.getquill.parser

import scala.quoted.{Type => TType, _}
import scala.quoted
import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}
import io.getquill.metaprog.Extractors._
import scala.reflect.ClassTag;
import scala.reflect.classTag;
import io.getquill.quat.Quat
import io.getquill.util.Messages
import io.getquill.ReturnAction
import scala.util.Try
import io.getquill.util.Format
import io.getquill.util.StringUtil.section
import io.getquill.parser.DoSerialize

trait BasicLiftable[T: ClassTag] extends ToExpr[T]:
  private[getquill] def liftOrThrow(element: T)(using Quotes): Expr[T] =
    import quotes.reflect._
    import io.getquill.util.Messages.qprint
    lift.lift(element).getOrElse {
      report.throwError(
        s"""|Could not Lift ${classTag[T].runtimeClass.getSimpleName} from the element:
            |${section(qprint(element).toString)}
            |"""".stripMargin
      )
    }

  def lift: (Quotes) ?=> PartialFunction[T, Expr[T]]
  def apply(element: T)(using Quotes): Expr[T] = liftOrThrow(element).asInstanceOf[Expr[T]]
  def unapply(t: T)(using Quotes) = Some(apply(t))
end BasicLiftable
