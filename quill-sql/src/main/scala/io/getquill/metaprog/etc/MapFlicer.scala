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

  private def buildClause[T: Type, PrepareRow: Type, Session: Type](core: Expr[T])(eachField: Expr[(String, String) => Boolean], map: Expr[Map[String, String]], default: Expr[String])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    ElaborateStructure.decomposedProductValueDetails[T](ElaborationSide.Encoding, UdtBehavior.Leaf) match
      case (terms, Structure.StructType.Leaf) => report.throwError("Not supported yet", core)
      case (terms, Structure.StructType.Node) =>
        val boolTerms =
          terms.map { (fieldString, isOptional, getter, tpe) =>
            val childTTerm = getter(core).asExprOf[Any]
            val mapSplice = '{ $map.getOrElse(${Expr[String](fieldString)}, $default) }

            def fieldInject(field: Expr[String]) =
              '{ $eachField( $field, ${LiftMacro.apply[String, PrepareRow, Session]( mapSplice )} )}

            // If the field is optional, the inner type has already been unpacked by `decomposedProductValueDetails` so we just use it
            if (isOptional)
              tpe match
                case '[inner] =>
                  '{ ${childTTerm.asExprOf[Option[inner]]}.exists(field => ${ fieldInject( '{ field.toString } ) }) }
            else
              fieldInject('{ $childTTerm.toString })
          }
        boolTerms.reduce((a, b) => '{ $a && $b })

  def base[T, PrepareRow, Session](using Quotes)(expr: Expr[T], map: Expr[Map[String, String]], default: Expr[String], eachField: Expr[(String, String) => Boolean])(using tpe: Type[T], pr: Type[PrepareRow], sess: Type[Session]): Expr[Boolean] = {
    import quotes.reflect._
    buildClause[T, PrepareRow, Session](expr)(eachField, map, default)
  }
}
