package io.getquill.metaprog

import io.getquill.generic._
import io.getquill._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.context.LiftMacro

object MapFilterator {
  inline def apply[T, PrepareRow](inline entity: T, inline map: Map[String, String], inline default:String, inline eachField: (String, String) => Boolean): Boolean = ${ applyImpl[T, PrepareRow]('entity, 'map, 'default, 'eachField) }
  def applyImpl[T: Type, PrepareRow: Type](entity: Expr[T], map: Expr[Map[String, String]], default: Expr[String], eachField: Expr[(String, String) => Boolean])(using qctx: Quotes): Expr[Boolean] = {
    val mp = new MapFilteratorMacro
    val ret = mp.base[T, PrepareRow](entity, map, default, eachField)
    ret
  }
}

class MapFilteratorMacro(using qctx: Quotes) {
  val ext = new TypeExtensions
  import ext._

  import qctx.reflect.{TypeRepr => TType, Term => TTerm, Ident => TIdent, _}

  def isProduct(tpe: Type[_]): Boolean =
      TType.of(using tpe) <:< TType.of[Product]

  private def recurse[T, PrepareRow, Fields, Types](id: TTerm, fieldsTup: Type[Fields], typesTup: Type[Types])(eachField: Expr[(String, String) => Boolean], map: Expr[Map[String, String]], default: Expr[String])(using baseType: Type[T], pr: Type[PrepareRow]): Expr[Boolean] = {
    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) =>
        val unsealedClassSymbol = TType.of(using baseType).widen.classSymbol
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
        // firstName == func(Map[String, String].getOrElse("firstName",null))
        val expr = '{ 
          $eachField( $childTTerm, ${LiftMacro.apply[String, PrepareRow]( mapSplice )} )
        }
        '{ ${expr} && ${recurse[T, PrepareRow, fields, types](id, Type.of[fields], Type.of[types])(eachField, map, default)(using baseType)} }

      case (_, '[EmptyTuple]) => '{ true }
      case _ => report.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T, PrepareRow](expr: Expr[T], map: Expr[Map[String, String]], default: Expr[String], eachField: Expr[(String, String) => Boolean])(using tpe: Type[T], pr: Type[PrepareRow]): Expr[Boolean] = {
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val result = recurse[T, PrepareRow, elementLabels, elementTypes](expr.asTerm, Type.of[elementLabels], Type.of[elementTypes])(eachField, map, default)(using tpe)
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
