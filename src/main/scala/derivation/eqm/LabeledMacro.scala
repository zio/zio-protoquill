package derivation.eqm

import scala.deriving._
import scala.quoted._
import scala.quoted.matching._
import scala.compiletime.{erasedValue, summonFrom, constValue}

trait Labeled[T] {
  def labels[T]: List[String]
}

object Labeled {
  def labels[Fields, Types](fieldsTpe: Type[Fields], typesTpe: Type[Types])(given qctx: QuoteContext): List[String] = {
    import qctx.tasty.{Type => TType, given, _}

    def typeIsConst[T](tpe: Type[T]) = // Don't need to check if label type is const, label types always must be const
      tpe.unseal.tpe match {
        case ConstantType(Constant(value)) => true
        case _ => false
      }

    def typeConstValue[T](tpe: Type[T]): String =
      tpe.unseal.tpe match {
        case ConstantType(Constant(value)) => value.toString
        // Macro error
      }

    def typeIsProduct[T](tpe: Type[T]): Boolean =
      tpe.unseal.tpe <:< '[Product].unseal.tpe

    (fieldsTpe, typesTpe) match {
      case ('[$field *: $fields], '[$tpe *: $types]) if typeIsProduct(tpe) =>
        val constValue: String = typeConstValue(field)
        println(s"Compile time label of product ${tpe.unseal} is ${constValue} - recursing")
        val insideFields = base(given tpe)
        insideFields.map(constValue + "." + _) ++ labels(fields, types)
        // One possiblity, if labelled can be summoned, continue the search?
        // summonExpr(given '[Labeled[$tpe]]) match {
        //   case Some(value) => base(given tpe)
        // }
      case ('[$field *: $fields], '[$tpe *: $types]) =>
        val constValue: String = typeConstValue(field)
        println("Compile time label: " + constValue)
        constValue :: labels(fields, types)
      case (_, '[Unit]) => Nil
    }
  }

  def base[T](given tpe: Type[T])(given qctx: QuoteContext): List[String] = {
    import qctx.tasty.{_, given}
    val ev: Expr[Mirror.Of[T]] = summonExpr(given '[Mirror.Of[$tpe]]).get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $elementLabels; type MirroredElemTypes = $elementTypes }} =>
        labels(elementLabels, elementTypes)
      
    }
  }

  def derived[T](given qctx: QuoteContext, tpe: Type[T]): List[String] = {
    base(given tpe)
  }
}

object LabeledMacro {
  inline def label[T]: List[String] = ${ labelImpl[T] }
  def labelImpl[T: Type](given qctx: QuoteContext): Expr[List[String]] = {
    Expr(Labeled.derived[T])
  }
}
