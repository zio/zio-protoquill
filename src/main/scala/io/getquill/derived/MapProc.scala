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
  def applyImpl[T: Type](entity: Expr[T], eachField: Expr[(String, String) => Boolean])(using qctx: QuoteContext): Expr[Boolean] = {
    val mp = new MapProcMacro
    val ret = mp.base(entity, eachField)
    ret
  }
}

class MapProcMacro(using qctx: QuoteContext) {
  val ext = new TypeExtensions
  import ext._

  import qctx.tasty.{Type => TType, Term => TTerm, Ident => TIdent, _}

  def isProduct(tpe: Type[_]): Boolean =
      tpe.unseal.tpe <:< '[Product].unseal.tpe

  private def recurse[T, Fields, Types](id: TTerm, fieldsTup: Type[Fields], typesTup: Type[Types])(eachField: Expr[(String, String) => Boolean])(using baseType: Type[T]): Expr[Boolean] = {
    (fieldsTup, typesTup) match {
      case ('[$field *: $fields], '[$tpe *: $types]) =>
        val unsealedClassSymbol = baseType.unseal.tpe.widen.classSymbol
        //println(s"Symbol: ${unsealedClassSymbol.get.show}")
        //println(s"Fields: ${unsealedClassSymbol.get.caseFields.map(_.show).toList}")
        val fieldString = field.constValue
        val fieldMethod = unsealedClassSymbol.get.caseFields.filter(field => field.name == fieldString).head
        val childTTerm = '{ (${Select(id, fieldMethod).seal}).toString }
        val expr = '{ $eachField(${childTTerm}, ${Expr(fieldString)}) }
        '{ ${expr} && ${recurse(id, fields, types)(eachField)(using baseType)} }

      case (_, '[EmptyTuple]) => '{ true }

      case _ => report.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
    }
  }

  def base[T](expr: Expr[T], eachField: Expr[(String, String) => Boolean])(using tpe: Type[T]): Expr[Boolean] = {
    Expr.summon(using '[Mirror.Of[$tpe]]) match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $elementLabels; type MirroredElemTypes = $elementTypes }} =>
            val result = recurse(expr.unseal, elementLabels, elementTypes)(eachField)(using tpe)
            result
          case _ =>
            report.throwError(s"Mirror for ${tpe.show} is not a product")
        }
      }
      case None => 
        report.throwError(s"Cannot derive a mirror for ${tpe.show}")
    }
  }
}
