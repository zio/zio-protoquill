package io.getquill.metaprog.etc

import io.getquill.generic._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{Hidden, Visible}
import scala.deriving._
import scala.quoted._
import io.getquill.context.LiftMacro
import io.getquill.metaprog.TypeExtensions._
import io.getquill.generic.ConstructType
import io.getquill.generic.DeconstructElaboratedEntityLevels
import io.getquill.generic.ElaborateStructure.{TermType, Leaf, Branch}
import io.getquill.generic.ElaborateStructure.UdtBehavior
import io.getquill.util.Format

// I.e. Map-Folding-Splicer since it recursively spliced clauses into a map
object MapFlicer {
  inline def apply[T, PrepareRow, Session](inline entity: T, inline map: Map[String, Any], inline default: Any, inline eachField: (Any, Any) => Boolean): Boolean =
    ${ applyImpl[T, PrepareRow, Session]('entity, 'map, 'default, 'eachField) }
  private def applyImpl[T: Type, PrepareRow: Type, Session: Type](entity: Expr[T], map: Expr[Map[String, Any]], default: Expr[Any], eachField: Expr[(Any, Any) => Boolean])(using Quotes): Expr[Boolean] = {
    val mp = new MapFlicerMacro
    val ret = mp.base[T, PrepareRow, Session](entity, map, default, eachField)
    ret
  }
}

object StringOrNull {
  def apply(v: Any): String = if (v == null) null else v.toString
}

/**
 * TODO This does not yet support Embedded objects. Need to work on that.
 */
class MapFlicerMacro {

  def isProduct(using Quotes)(tpe: Type[_]): Boolean =
    import quotes.reflect._
    TypeRepr.of(using tpe) <:< TypeRepr.of[Product]

  private def buildClause[T: Type, PrepareRow: Type, Session: Type](core: Expr[T])(eachField: Expr[(Any, Any) => Boolean], map: Expr[Map[String, Any]], default: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    ElaborateStructure.decomposedProductValueDetails[T](ElaborationSide.Encoding, UdtBehavior.Leaf) match
      case (terms, Leaf) => report.throwError("Not supported yet", core)
      case (terms, Branch) =>
        val boolTerms =
          terms.map { (fieldString, isOptional, getter, tpe) =>
            val childTTerm = getter(core)
            val actualChildType =
              if (isOptional)
                childTTerm.asTerm.tpe.asType match
                  case '[Option[t]] => TypeRepr.of[t]
              else
                childTTerm.asTerm.tpe

            // Note usually `default` is null and cannot do asExprOf[T] on it otherwise will get a `of type: scala.Any... did not conform to type: java.lang.String/Int/etc...` exception
            def mapSplice = '{ $map.getOrElse(${ Expr[String](fieldString) }, ${ default }) }
            def fieldInject[T](field: Expr[T])(using Type[T]) =
              val printType = Expr(Format.TypeOf[T].toString)
              val printTerm = Expr(Format.Expr(childTTerm).toString)
              '{
                $eachField(
                  $field,
                  ${
                    LiftMacro.valueOrString[T, PrepareRow, Session](mapSplice)
                  }
                )
              }

            // If the field is optional, the inner type has already been unpacked by `decomposedProductValueDetails` so we just use it
            tpe match
              case '[inner] =>
                // Assuming: actualChildType <:< TypeRepr.of[inner]
                if (isOptional)
                  '{
                    ${ childTTerm.asExprOf[Option[inner]] }.exists(field =>
                      ${ fieldInject[inner]('{ field }) }
                    )
                  }
                else
                  fieldInject[inner](childTTerm.asExprOf[inner])
          }
        boolTerms.reduce((a, b) => '{ $a && $b })

  def base[T, PrepareRow, Session](using
      Quotes
  )(expr: Expr[T], map: Expr[Map[String, Any]], default: Expr[Any], eachField: Expr[(Any, Any) => Boolean])(using tpe: Type[T], pr: Type[PrepareRow], sess: Type[Session]): Expr[Boolean] = {
    import quotes.reflect._
    buildClause[T, PrepareRow, Session](expr)(eachField, map, default)
  }
}
