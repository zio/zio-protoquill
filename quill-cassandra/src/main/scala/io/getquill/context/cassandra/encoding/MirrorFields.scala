package io.getquill.context.cassandra.encoding

import scala.deriving._
import scala.quoted._
import io.getquill.util.Format

object MirrorFields:
  import io.getquill.metaprog.TypeExtensions._

  private def recurseCollect[Fields: Type, Types: Type](fieldsTup: Type[Fields], typesTup: Type[Types])(using Quotes): List[(String, Type[_])] =
    import quotes.reflect._
    (fieldsTup, typesTup) match
      case ('[field *: fields], '[tpe *: types]) =>
        val fieldValue = Type.of[field].constValue
        (fieldValue, Type.of[tpe]) :: recurseCollect[fields, types](Type.of[fields], Type.of[types])
      case (_, '[EmptyTuple]) => Nil
      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)

  def of[T: Type](using Quotes): (Expr[Mirror.ProductOf[T]], List[(String, Type[_])]) =
    import quotes.reflect._
    Expr.summon[Mirror.Of[T]] match
      case Some(ev) =>
        ev match
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            (m, recurseCollect[elementLabels, elementTypes](Type.of[elementLabels], Type.of[elementTypes]))
          case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            report.throwError(s"The detected type of ${Format.TypeOf[T]} is a Sum (i.e. Enum or Sealed trait hierarchy. Only Product-type (i.e. Case-Class) UDTs are supported.")
      case None =>
        val traces = Thread.currentThread.getStackTrace.take(50).map("  " + _.toString).mkString("\n")
        report.throwError(s"Could not detect mirror for: ${Format.TypeOf[T]}")
end MirrorFields