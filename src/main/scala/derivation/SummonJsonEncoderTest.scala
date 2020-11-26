package derivation

import scala.deriving._
import scala.quoted._
import scala.compiletime.{erasedValue, summonFrom, summonInline, constValue}
import JsonEncoder._
import scala.reflect.ClassTag

object SummonJsonEncoderTest {

  inline def summonMirror[T]: Option[Mirror.Of[T]] =
    summonFrom {
      case m as given Mirror.Of[T] => Some(m)
      case _ => None
    }

  inline def usingSummonFrom[T](value: =>T): String = 
    ${ usingSummonFromImpl('value, summonMirror[T]) } // TODO paramters must be quoted. Could try summoning the Expr[Mirror.Of[T]] instead

  def usingSummonFromImpl[T: Type](value: Expr[T], m: Option[Mirror.Of[T]])(using Quotes): Expr[String] = {
    import quotes.reflect._

    val theMirror = m match { case Some(mirror) => mirror}

    theMirror match {
      case m: Mirror.ProductOf[T] => println("it's a product: " + mirrorFields[m.MirroredElemLabels])
    }

    '{ "Doesn't matter" }
  }

  inline def classTag[T] = summonInline[ClassTag[T]]

  inline def mirrorFields[Fields <: Tuple]: List[String] = 
    inline erasedValue[Fields] match {
      case _: (field *: fields) => constValue[field].toString :: mirrorFields[fields]
      case _ => Nil
    }

  inline def usingSummonExpr[T](value: =>T): String = ${ usingSummonExprImpl('value) }

  def usingSummonExprImpl[T: Type](value: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect._

    val mirrorExpr = Expr.summon[Mirror.Of[T]] match {
      case Some(mirror) => mirror
    }

    '{
      given JsonEncoder[T] = JsonEncoder.derived($mirrorExpr)
      val encoder = summon[JsonEncoder[T]]
      encoder.encode($value) //hellooo
    }
  }
}