package io.getquill.metaprog.etc

import io.getquill.generic._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.context.LiftMacro
import io.getquill.metaprog.TypeExtensions._
import io.getquill.generic.ConstructType
import io.getquill.generic.DeconstructElaboratedEntityLevels
import io.getquill.generic.Structure.StructType
import io.getquill.generic.Structure.StructType.{ Leaf, Node }
import io.getquill.generic.ElaborateStructure.UdtBehavior

object ColumnsFlicer {
  inline def apply[T, PrepareRow, Session](inline entity: T, inline columns: List[String]): T = ${ applyImpl[T, PrepareRow, Session]('entity, 'columns) }
  private def applyImpl[T: Type, PrepareRow: Type, Session: Type](entity: Expr[T], columns: Expr[List[String]])(using Quotes): Expr[T] = {
    new ColumnsFlicerMacro().base[T, PrepareRow, Session](entity, columns)
  }
}

class ColumnsFlicerMacro {
  def isProduct(using Quotes)(tpe: Type[_]): Boolean =
    import quotes.reflect._
    TypeRepr.of(using tpe) <:< TypeRepr.of[Product]

  private def recurse[T, PrepareRow, Session, Fields, Types](using Quotes)(id: quotes.reflect.Term, fieldsTup: Type[Fields], typesTup: Type[Types])(columns: Expr[List[String]])(using baseType: Type[T], pr: Type[PrepareRow], sess: Type[Session]): List[(Type[_], Expr[_])] = {
    import quotes.reflect._
    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) =>
        val unsealedClassSymbol = TypeRepr.of(using baseType).widen.classSymbol
        // String representing the field being summoned e.g. "firstName" of Person
        val fieldString = Type.of[field].constValue
        // method of the summoned field
        val fieldMethod = unsealedClassSymbol.get.caseFields.filter(field => field.name == fieldString).head
        // 'invocation' of the found method e.g. p.name
        val childTTerm = '{ (${ Select(id, fieldMethod).asExprOf[tpe] }) }

        if (Expr.summon[GenericDecoder[_, Session, tpe, DecodingType.Specific]].isDefined) then
          // TODO Maybe use ==1 versus 'true' in this case. See how this plays out with VendorizeBooleans behavior
          val liftClause = '{ $columns.contains(${Expr(fieldString)}) }
          val liftedCondition = LiftMacro.apply[Boolean, PrepareRow, Session]( liftClause )
          val columnSplice: Expr[tpe] = '{ if ($liftedCondition) $childTTerm else null.asInstanceOf[tpe] }
          // construction of the comparison term:
          // if (lift(func(List[String].contains("firstName")))) person.firstName else null
          val expr = (Type.of[tpe], columnSplice)
          val rec = recurse[T, PrepareRow, Session, fields, types](id, Type.of[fields], Type.of[types])(columns)(using baseType)
          expr +: rec
        else
          // TODO Recursive delving for optional product types
          // inner class construct e.g. case class Person(name: Name, age: Int),  case class Name(first: String, last: String)
          // so this property would be p.name in a query query[Person].map(p => Person(Name({p.name}.first, {p.name}.last), ...)
          val subMapping = base[tpe, PrepareRow, Session](childTTerm, columns)
          val expr = (Type.of[tpe], subMapping)
          val rec = recurse[T, PrepareRow, Session, fields, types](id, Type.of[fields], Type.of[types])(columns)(using baseType)
          expr +: rec

      case (_, '[EmptyTuple]) => Nil
      case _ => report.throwError("Cannot generically derive Types In Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T, PrepareRow, Session](using Quotes)(expr: Expr[T], columns: Expr[List[String]])(using tpe: Type[T], pr: Type[PrepareRow], sess: Type[Session]): Expr[T] = {
    import quotes.reflect._
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val fields = recurse[T, PrepareRow, Session, elementLabels, elementTypes](expr.asTerm, Type.of[elementLabels], Type.of[elementTypes])(columns)(using tpe)
            ConstructType[T](m, fields)
          case _ =>
            report.throwError(s"Mirror for ${Type.of[T]} is not a product")
        }
      }
      case None =>
        report.throwError(s"Cannot derive a mirror for ${Type.of[T]}")
    }
  }
}