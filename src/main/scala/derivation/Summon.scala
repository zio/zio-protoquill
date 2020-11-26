package derivation

import scala.deriving._
import scala.compiletime.erasedValue

object SummonCsvEncoderTest {
  import scala.compiletime.summonFrom

  inline def summonMirrorAndUseEncoder[T](value: =>T): String = {
    val mirror = summonFrom {
      case m: Mirror.ProductOf[T] => m
    }
    CsvEncoder.derived(mirror).encode(value)
  }
}

object SummonCsvEncoderManualTest {
  //import scala.compiletime.summonFrom
  import scala.quoted._
  
  import scala.compiletime.{erasedValue, summonFrom}

  inline def encodeElem[T](elem: T): String = summonFrom {
    case encoder: CsvEncoder[T] => encoder.encode(elem)
  }

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) => 
        encodeElem[elem](productElement[elem](value, idx)) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): CsvEncoder[T] = new CsvEncoder[T] {
      def encode(value: T): String = 
        inline ev match {
          case m: Mirror.ProductOf[T] =>
            encodeElems[m.MirroredElemTypes](0)(value).mkString(", ")
        }
    }

  // TODO Can we do the same thing with a sum? I.e. can our top-level element be polymorphic?
  inline def summonMirrorAndManuallyEncode[T](value: =>T): String = {
    summonFrom {
      //case m: Mirror.ProductOf[T] => encodeElems[m.MirroredElemTypes](0)(value).mkString(", ")
      case m: Mirror.ProductOf[T] => derived(m).encode(value)
      case _ => "cannot get mirror"
    }
  }

  // inline def summonJsonEncoder[T](value: =>T): String = {
  //   summonFrom {
  //     case enc: JsonEncoder[T] => "foo"
  //     case _ => "bar"
  //   }
  // }

  // inline def summonTest[T](value: =>T): String = ${ summonTestImpl[T]('value) }
  // def summonTestImpl[T: Type](body: Expr[T])(using Quotes): Expr[String] = {
  //   import quotes.reflect._

  //   val from = Expr.summon {
  //     case m :Mirror.Of[T] => Some(m)
  //     case _ => None
  //   }

  //   val mirror = Expr.summon[Mirror.Of[T]] match {
  //     case Some(mirror) => println("mirror: " + new AstPrinter().apply(Term.of(mirror)))
  //     case None =>
  //   }

  //   '{"hello"}
  // }
}





object SummonJsonEncoderManualTest {
  //import scala.compiletime.summonFrom
  import scala.quoted._
  
  import scala.compiletime.{erasedValue, summonFrom}

  inline def encodeElem[T](elem: T): String = summonFrom {
    case encoder: JsonEncoder[T] => encoder.encode(elem)
  }

  inline def encodeElems[Elems <: Tuple](idx: Int)(value: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) => 
        encodeElem[elem](productElement[elem](value, idx)) :: encodeElems[elems1](idx + 1)(value)
      case _ => Nil
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): JsonEncoder[T] = new JsonEncoder[T] {
    def encode(value: T): String = 
      inline ev match {
        case m: Mirror.SumOf[T] =>
          "not supporting this case yet"
        case m: Mirror.ProductOf[T] =>
          val elems = encodeElems[m.MirroredElemTypes](0)(value)
          val labels = value.asInstanceOf[Product].productElementNames
          val keyValues = labels.zip(elems).map((k, v) => s"$k: $v")
          "{" + (keyValues).mkString(", ") + "}"
        case other =>
          throw new RuntimeException("mirror was an invalid value: " + other)
      }
  }

  inline def summonMirrorAndManuallyEncode[T](value: =>T): String = {
    summonFrom {
      //case m: Mirror.ProductOf[T] => encodeElems[m.MirroredElemTypes](0)(value).mkString(", ")
      case m: Mirror.ProductOf[T] => derived(m).encode(value)
      case _ => "cannot get mirror"
    }
  }
}
