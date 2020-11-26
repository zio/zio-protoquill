package io.getquill.derived

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._

object MapProc {
  inline def apply[T](inline entity: T, inline eachField: (String, String) => Boolean): Boolean = ${ applyImpl('entity, 'eachField) }
  def applyImpl[T: Type](entity: Expr[T], eachField: Expr[(String, String) => Boolean])(using qctx: Quotes): Expr[Boolean] = {
    val mp = new MapProcMacro
    val ret = mp.base(entity, eachField)
    ret
  }
}

class MapProcMacro(using qctx: Quotes) {
  val ext = new TypeExtensions
  import ext._

  import qctx.reflect.{TypeRepr => TType, Term => TTerm, Ident => TIdent, _}

  def isProduct(tpe: Type[_]): Boolean =
      TType.of(using tpe) <:< TType.of[Product]

  private def recurse[T, Fields, Types](id: TTerm, fieldsTup: Type[Fields], typesTup: Type[Types])(eachField: Expr[(String, String) => Boolean])(using baseType: Type[T]): Expr[Boolean] = {
    (fieldsTup, typesTup) match {
      case ('[field *: fields], '[tpe *: types]) =>
        val unsealedClassSymbol = TType.of(using baseType).widen.classSymbol
        //println(s"Symbol: ${unsealedClassSymbol.get.show}")
        //println(s"Fields: ${unsealedClassSymbol.get.caseFields.map(_.show).toList}")
        val fieldString = Type.of[field].constValue
        val fieldMethod = unsealedClassSymbol.get.caseFields.filter(field => field.name == fieldString).head
        val childTTerm = '{ (${ Select(id, fieldMethod).asExprOf[Any] }).toString }
        val expr = '{ $eachField(${childTTerm}, ${Expr[String](fieldString)}) }
        '{ ${expr} && ${recurse(id, Type.of[fields], Type.of[types])(eachField)(using baseType)} }

      case (_, '[EmptyTuple]) => '{ true }

      case _ => report.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T](expr: Expr[T], eachField: Expr[(String, String) => Boolean])(using tpe: Type[T]): Expr[Boolean] = {
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val result = recurse(TTerm.of(expr), Type.of[elementLabels], Type.of[elementTypes])(eachField)(using tpe)
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
