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
import io.getquill.ast
import io.getquill.quat
import io.getquill.util.CommonExtensions.Throwable._
import io.getquill.util.Messages.qprint

object Lifters:

  trait Proxy:
    def default: Lifter

    def apply(ast: Ast): Quotes ?=> Expr[Ast] = default.liftAst(ast) // can also do ast.lift but this makes some error messages simpler
    def action(ast: Action): Quotes ?=> Expr[Action] = default.liftAction(ast)
    def assignment(ast: Assignment): Quotes ?=> Expr[Assignment] = default.liftAssignment(ast)
    def entity(ast: Entity): Quotes ?=> Expr[Entity] = default.liftEntity(ast)
    def tuple(ast: Tuple): Quotes ?=> Expr[Tuple] = default.liftTuple(ast)
    def caseClass(ast: CaseClass): Quotes ?=> Expr[CaseClass] =
      // Need to use lift directly since using liftCaseClass.apply serializes to Ast which can result
      // in: Expecting io.getquill.ast.CaseClass but got io.getquill.ast.Ast errors. It is hard to understand
      // why that is the case but likely because of how the '{ EagerEntitiesPlanter(..., caseClassAst) } interprets
      // the caseClassAst variable before extracting it from the quotation.
      default.liftCaseClass.lift(ast)

    def ident(ast: AIdent): Quotes ?=> Expr[AIdent] = default.liftIdent(ast)
    def quat(quat: Quat): Quotes ?=> Expr[Quat] = default.liftQuat(quat)
    def returnAction(returnAction: ReturnAction): Quotes ?=> Expr[ReturnAction] = default.liftReturnAction(returnAction)

    def scalarTag(v: ScalarTag): Quotes ?=> Expr[ScalarTag] = default.liftScalarTag(v)
    def quotationTag(v: QuotationTag): Quotes ?=> Expr[QuotationTag] = default.liftQuotationTag(v)
  end Proxy

  trait WithSerializing[T <: SerBase, SerBase]:
    protected def tryLiftSerialized(element: T)(using Quotes, TType[T]): Option[Expr[T]]
    protected def tryOrWarn(element: T, tryToSerializeElement: => Expr[T])(using Quotes, TType[T]): Option[Expr[T]] =
      try {
        Some(tryToSerializeElement)
      } catch {
        case e: Throwable =>
          orWarn(element, e)
          None
      }
    protected def orWarn(element: T, e: Throwable)(using Quotes): Unit = {
      val msg =
        s"""Could not unift-serialize the '${element.getClass}':
          |${io.getquill.util.Messages.qprint(element)}.
          |Performing a regular unlift instead. Due to exception:
          |${e.stackTraceToString}
          |""".stripMargin
      println(s"WARNING: ${msg}")
      quotes.reflect.report.warning(msg)
    }

    // Convenience method for implementors to use
    protected def hasSerializeDisabledTypeclass(using Quotes): Boolean =
      import quotes.reflect._
      Expr.summon[DoSerialize] match
        case Some(SerialHelper.BehaviorEnabled()) =>
          true
        case Some(value) =>
          false
        case _ =>
          false
  end WithSerializing

  object WithSerializing:
    trait Ast[T <: ast.Ast] extends WithSerializing[T, ast.Ast]:
      private val astToExpr = new SerialHelper.Ast.Expr[T]
      protected def tryLiftSerialized(element: T)(using Quotes, TType[T]): Option[Expr[T]] =
        tryOrWarn(element, astToExpr(element))

    trait Quat[T <: quat.Quat] extends WithSerializing[T, quat.Quat]:
      private val quatToExpr = new SerialHelper.Quat.Expr[T]
      protected def tryLiftSerialized(element: T)(using Quotes, TType[T]): Option[Expr[T]] =
        tryOrWarn(element, quatToExpr(element))
  end WithSerializing

  trait Plain[T: ClassTag] extends ToExpr[T]:
    def lift: Quotes ?=> PartialFunction[T, Expr[T]]

    // The primary entry-point for external usage
    def orFail(element: T)(using Quotes): Expr[T] =
      liftPlainOrFail(element)

    private[getquill] def liftPlainOrFail(element: T)(using Quotes): Expr[T] =
      import quotes.reflect._
      import io.getquill.util.Messages.qprint
      lift.lift(element).getOrElse {
        report.throwError(failMsg(element))
      }

    protected def failMsg(element: T) =
      s"""|Could not Lift ${classTag[T].runtimeClass.getSimpleName} from the element:
          |${section(qprint(element).toString)}
          |"""".stripMargin

    def unapply(element: T)(using Quotes) = Some(orFail(element))
    def apply(element: T)(using Quotes): Expr[T] = orFail(element)
  end Plain

  object Plain:
    trait Ast[T: ClassTag] extends Plain[T]:
      override protected def failMsg(element: T) =
        s"Could not Lift AST type ${classTag[T].runtimeClass.getSimpleName} from the element:\n" +
          s"${section(io.getquill.util.Messages.qprint(element).toString)}\n" +
          s"of the Quill Abstract Syntax Tree"

    trait Quat[T: ClassTag] extends Plain[T]:
      override protected def failMsg(element: T) =
        s"Could not Lift AST Quat-type ${classTag[T].runtimeClass.getSimpleName} from the element:\n" +
          s"${section(io.getquill.util.Messages.qprint(element).toString)}\n" +
          s"of the Quill (Quat) Abstract Syntax Tree"
  end Plain

end Lifters
