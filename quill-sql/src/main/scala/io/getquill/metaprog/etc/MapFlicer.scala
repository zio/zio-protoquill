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


object ColumnsFlicer {
  inline def apply[T, PrepareRow, Session](inline entity: T, inline columns: List[String]): T = ${ applyImpl[T, PrepareRow, Session]('entity, 'columns) }
  private def applyImpl[T: Type, PrepareRow: Type, Session: Type](entity: Expr[T], columns: Expr[List[String]])(using Quotes): Expr[T] = {
    val mp = new ColumnsFlicerMacro
    // for case class Person(name: String, age: Int)
    // List('{  })
    val (mirror, fields) = mp.base[T, PrepareRow, Session](entity, columns)
    val caseClassConstructions = ConstructType[T](mirror, fields)
    caseClassConstructions
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
        // 'invocation' of the found method
        val childTTerm = '{ (${ Select(id, fieldMethod).asExprOf[tpe] }) }
        // TODO Maybe use ==1 versus 'true' in this case. See how this plays out with VendorizeBooleans behavior
        val liftClause = '{ $columns.contains(${Expr(fieldString)}) }
        val liftedCondition = LiftMacro.apply[Boolean, PrepareRow, Session]( liftClause )
        val columnSplice: Expr[tpe] = '{ if ($liftedCondition) $childTTerm else null.asInstanceOf[tpe] }
        // construction of the comparison term:
        // if (lift(func(List[String].contains("firstName")))) person.firstName else null
        val expr = (Type.of[tpe], columnSplice)
        val rec = recurse[T, PrepareRow, Session, fields, types](id, Type.of[fields], Type.of[types])(columns)(using baseType)
        expr +: rec

      case (_, '[EmptyTuple]) => Nil
      case _ => report.throwError("Cannot generically derive Types In Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T, PrepareRow, Session](using Quotes)(expr: Expr[T], columns: Expr[List[String]])(using tpe: Type[T], pr: Type[PrepareRow], sess: Type[Session]): (Expr[Mirror.ProductOf[T]], List[(Type[_], Expr[_])]) = {
    import quotes.reflect._
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val result = recurse[T, PrepareRow, Session, elementLabels, elementTypes](expr.asTerm, Type.of[elementLabels], Type.of[elementTypes])(columns)(using tpe)
            (m, result)
          case _ =>
            report.throwError(s"Mirror for ${Type.of[T]} is not a product")
        }
      }
      case None =>
        report.throwError(s"Cannot derive a mirror for ${Type.of[T]}")
    }
  }
}


// I.e. Map-Folding-Splicer since it recursively spliced clauses into a map
object MapFlicer {
  inline def apply[T, PrepareRow, Session](inline entity: T, inline map: Map[String, String], inline default:String, inline eachField: (String, String) => Boolean): Boolean = ${ applyImpl[T, PrepareRow, Session]('entity, 'map, 'default, 'eachField) }
  private def applyImpl[T: Type, PrepareRow: Type, Session: Type](entity: Expr[T], map: Expr[Map[String, String]], default: Expr[String], eachField: Expr[(String, String) => Boolean])(using Quotes): Expr[Boolean] = {
    val mp = new MapFlicerMacro
    val ret = mp.base[T, PrepareRow, Session](entity, map, default, eachField)
    ret
  }
}

/**
 * TODO This does not yet support Embedded objects. Need to work on that.
 */
class MapFlicerMacro {

  def isProduct(using Quotes)(tpe: Type[_]): Boolean =
    import quotes.reflect._
    TypeRepr.of(using tpe) <:< TypeRepr.of[Product]

  private def recurse[T, PrepareRow, Session, Fields, Types](using Quotes)(id: quotes.reflect.Term, fieldsTup: Type[Fields], typesTup: Type[Types])(eachField: Expr[(String, String) => Boolean], map: Expr[Map[String, String]], default: Expr[String])(using baseType: Type[T], pr: Type[PrepareRow], sess: Type[Session]): Expr[Boolean] = {
    import quotes.reflect._
    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) =>
        val unsealedClassSymbol = TypeRepr.of(using baseType).widen.classSymbol
        //println(s"Symbol: ${unsealedClassSymbol.get.show}")
        //println(s"Fields: ${unsealedClassSymbol.get.caseFields.map(_.show).toList}")
        // String representing the field being summoned e.g. "firstName" of Person
        val fieldString = Type.of[field].constValue
        // method of the summoned field
        val fieldMethod = unsealedClassSymbol.get.caseFields.filter(field => field.name == fieldString).head
        // 'invocation' of the found method
        val childTTerm = '{ (${ Select(id, fieldMethod).asExprOf[Any] }).toString }
        val mapSplice = '{ $map.getOrElse(${Expr[String](fieldString)}, $default) }
        // construction of the comparison term:
        // lift(firstName == func(Map[String, String].getOrElse("firstName",null)))
        val expr = '{
          $eachField( $childTTerm, ${LiftMacro.apply[String, PrepareRow, Session]( mapSplice )} )
        }
        val rec = recurse[T, PrepareRow, Session, fields, types](id, Type.of[fields], Type.of[types])(eachField, map, default)(using baseType)
        '{ $expr && $rec }

      case (_, '[EmptyTuple]) => '{ true }
      case _ => report.throwError("Cannot generically derive Types In Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T, PrepareRow, Session](using Quotes)(expr: Expr[T], map: Expr[Map[String, String]], default: Expr[String], eachField: Expr[(String, String) => Boolean])(using tpe: Type[T], pr: Type[PrepareRow], sess: Type[Session]): Expr[Boolean] = {
    import quotes.reflect._
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val result = recurse[T, PrepareRow, Session, elementLabels, elementTypes](expr.asTerm, Type.of[elementLabels], Type.of[elementTypes])(eachField, map, default)(using tpe)
            result
          case _ =>
            report.throwError(s"Mirror for ${Type.of[T]} is not a product")
        }
      }
      case None =>
        report.throwError(s"Cannot derive a mirror for ${Type.of[T]}")
    }
  }
}
