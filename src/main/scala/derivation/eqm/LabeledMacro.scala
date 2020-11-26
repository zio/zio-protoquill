package derivation.eqm

import scala.deriving._
import scala.quoted._

import scala.compiletime.{erasedValue, summonFrom, constValue}

trait Labeled[T] {
  def labels[T]: List[String]
}

object Labeled {
  def labels[Fields: Type, Types: Type](using Quotes): List[String] = {
    import quotes.reflect._

    def typeIsConst[T: Type] = // Don't need to check if label type is const, label types always must be const
      TypeRepr.of[T] match {
        case ConstantType(Constant.Int(value)) => true
        case ConstantType(Constant.String(value)) => true
        case _ => false
      }

    def typeConstValue[T: Type]: String =
      TypeRepr.of[T] match {
        case ConstantType(Constant.Int(value)) => value.toString
        case ConstantType(Constant.String(value)) => value.toString
        // Macro error
      }

    def typeIsProduct[T: Type]: Boolean =
      TypeRepr.of[T] <:< TypeRepr.of[Product]

    (Type.of[Fields], Type.of[Types]) match {
      case ('[field *: fields], '[tpe *: types]) if typeIsProduct[tpe] =>
        val constValue: String = typeConstValue[field]
        println(s"Compile time label of product ${TypeRepr.of[tpe]} is ${constValue} - recursing")
        val insideFields = base[tpe]
        insideFields.map(constValue + "." + _) ++ labels[fields, types]
        // One possiblity, if labelled can be summoned, continue the search?
        // Expr.summon[Labeled[tpe]] match {
        //   case Some(value) => base(using tpe)
        // }
      case ('[field *: fields], '[tpe *: types]) =>
        val constValue: String = typeConstValue[field]
        println("Compile time label: " + constValue)
        constValue :: labels[fields, types]
      case (_, '[EmptyTuple]) => Nil
    }
  }

  def base[T: Type](using Quotes): List[String] = {
    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    ev match {
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
        labels[elementLabels, elementTypes]
      
    }
  }

  def apply[T: Type](using Quotes): List[String] = {
    base[T]
  }
}

object LabeledMacro {
  inline def label[T]: List[String] = ${ labelImpl[T] }
  def labelImpl[T: Type](using Quotes): Expr[List[String]] = {
    Expr(Labeled[T])
  }
}
