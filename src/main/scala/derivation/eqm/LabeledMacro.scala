package derivation.eqm

import scala.deriving._
import scala.quoted._

import scala.compiletime.{erasedValue, summonFrom, constValue}

trait Labeled[T] {
  def labels[T]: List[String]
}

object Labeled {
  def labels[Fields, Types](fieldsTpe: Type[Fields], typesTpe: Type[Types])(using qctx: QuoteContext): List[String] = {
    import qctx.tasty.{Type => TType, _}

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
        val insideFields = base(using tpe)
        insideFields.map(constValue + "." + _) ++ labels(fields, types)
        // One possiblity, if labelled can be summoned, continue the search?
        // Expr.summon(using '[Labeled[$tpe]]) match {
        //   case Some(value) => base(using tpe)
        // }
      case ('[$field *: $fields], '[$tpe *: $types]) =>
        val constValue: String = typeConstValue(field)
        println("Compile time label: " + constValue)
        constValue :: labels(fields, types)
      case (_, '[EmptyTuple]) => Nil
    }
  }

  def base[T](using tpe: Type[T])(using qctx: QuoteContext): List[String] = {
    import qctx.tasty.{_}
    val ev: Expr[Mirror.Of[T]] = Expr.summon(using '[Mirror.Of[$tpe]]).get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $elementLabels; type MirroredElemTypes = $elementTypes }} =>
        labels(elementLabels, elementTypes)
      
    }
  }

  def apply[T](using qctx: QuoteContext, tpe: Type[T]): List[String] = {
    base(using tpe)
  }
}

object LabeledMacro {
  inline def label[T]: List[String] = ${ labelImpl[T] }
  def labelImpl[T: Type](using qctx: QuoteContext): Expr[List[String]] = {
    Expr(Labeled[T])
  }
}
