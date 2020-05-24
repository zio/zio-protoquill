package derivation

import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import scala.quoted._
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class Collector {
  val fields: ArrayBuffer[(String, ClassTag[_])] = new ArrayBuffer()
  def add(fieldName: String, ct: ClassTag[_]): Collector = {
    fields += ((fieldName, ct))
    this
  }
}

trait FieldCollector[T] {
  def collect(fields: Collector): Collector
}

object FieldCollector {

  inline def collectFromChild[T](fields: Collector): Collector =
    summonFrom {
      case fc: FieldCollector[T] => fc.collect(fields)
    }

  inline def isSum[T] =
    summonFrom {
      case given m: Mirror.SumOf[T] => true
      case _ => false
    }

  inline def isProduct[T] =
    summonFrom {
      case given m: Mirror.ProductOf[T] => true
      case _ => false
    }

  inline def className[T] =
    summonFrom {
      case given ct: ClassTag[T] => ct.toString
      case _ => "<UNKNOWN>"
    }

  inline def classTag[T] =
    summonFrom {
      case given ct: ClassTag[T] => ct
    }

  type ProductType[T <: Product] = T

  inline def traverseCoElements[Fields <: Tuple, Types <: Tuple](fields: Collector): Collector = {
    inline erasedValue[(Fields, Types)] match {
      // Recurse if there is a sum type
      case (_: (field *: fields), _:(tpe *: types)) if isSum[tpe] =>
        println(s"Traverse sub fields of: ${constValue[field]}: ${className[tpe]}")
        traverseCoElements[fields, types](collectFromChild[tpe](fields))

      case (_: (field *: fields), _:(ProductType[tpe] *: types)) if isProduct[tpe] /*if isProduct[tpe] not needed */ =>
        println(s"Traverse product field of: ${constValue[field]}: ${className[tpe]}")
        traverseCoElements[fields, types](collectFromChild[tpe](fields))

      case _ => fields
    }
  }

  inline def collectFromProduct[Fields <: Tuple, Types <: Tuple](fields: Collector): Collector = {
    inline erasedValue[(Fields, Types)] match {
      case (_: (field *: fields), _: (tpe *: types)) => 
        collectFromProduct[fields, types](fields.add(constValue[field].toString, classTag[tpe]))
      case _ =>
        fields
    }
  }

  inline def derived[T](using ev: Mirror.Of[T], ct: ClassTag[T]): FieldCollector[T] = new FieldCollector[T] {
    def collect(fields: Collector): Collector = {
      inline ev match {
        case m: Mirror.SumOf[T] => 
          println("Mirror.SumOf: " + ct)
          traverseCoElements[m.MirroredElemLabels, m.MirroredElemTypes](fields)
        case m: Mirror.ProductOf[T] => 
          println("Mirror.ProductOf: " + ct)
          collectFromProduct[m.MirroredElemLabels, m.MirroredElemTypes](fields)
      }
    }
  }
}