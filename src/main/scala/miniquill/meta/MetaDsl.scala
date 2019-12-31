package miniquill.meta

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}

trait Encoder {

}

object MetaDsl {
  

  class ValueComputation(given qctx: QuoteContext) {
    import qctx.tasty._

    //private def caseClassConstructor(t: Type) =
      
  }

  inline def playAround[T](stuff: T): Any = ${ playAroundImpl('stuff) }
  def playAroundImpl[T: Type](stuff: Expr[T])(given qctx: QuoteContext): Expr[Any] = {
    null
  }


}


// trait MirrorDecoders extends EncodingDsl {

//   override type PrepareRow = Row
//   override type ResultRow = Row
//   override type DecoderType[T] = MirrorDecoder[T]

//   case class MirrorDecoder[T](decoder: BaseDecoder[T]) extends BaseDecoder[T] {
//     override def apply(index: Int, row: ResultRow) =
//       decoder(index, row)
//   }

//   def decoder[T: ClassTag]: Decoder[T] = MirrorDecoder((index: Int, row: ResultRow) => row[T](index))

//   def decoderUnsafe[T]: Decoder[T] = MirrorDecoder((index: Int, row: ResultRow) => row.data(index).asInstanceOf[T])

//   //implicit def mappedDecoder[I, O](implicit mapped: MappedEncoding[I, O], d: Decoder[I]): Decoder[O] =
//   //  MirrorDecoder((index: Index, row: ResultRow) => mapped.f(d.apply(index, row)))

//   implicit def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
//     MirrorDecoder((index: Int, row: ResultRow) =>
//       row[Option[Any]](index) match {
//         case Some(v) => Some(d(0, Row(v)))
//         case None    => None
//       })

//   implicit val stringDecoder: Decoder[String] = decoder[String]
//   implicit val bigDecimalDecoder: Decoder[BigDecimal] = decoder[BigDecimal]
//   implicit val booleanDecoder: Decoder[Boolean] = decoder[Boolean]
//   implicit val byteDecoder: Decoder[Byte] = decoder[Byte]
//   implicit val shortDecoder: Decoder[Short] = decoder[Short]
//   implicit val intDecoder: Decoder[Int] = decoder[Int]
//   implicit val longDecoder: Decoder[Long] = decoder[Long]
//   implicit val floatDecoder: Decoder[Float] = decoder[Float]
//   implicit val doubleDecoder: Decoder[Double] = decoder[Double]
//   implicit val byteArrayDecoder: Decoder[Array[Byte]] = decoder[Array[Byte]]
//   // implicit val dateDecoder: Decoder[Date] = decoder[Date]
//   // implicit val localDateDecoder: Decoder[LocalDate] = decoder[LocalDate]
//   // implicit val uuidDecoder: Decoder[UUID] = decoder[UUID]
// }



object DecoderDsl extends EncodingDsl {

  case class MirrorDecoder[T](decoder: Decoder[T]) extends Decoder[T] {
    override def apply(index: Int, row: ResultRow) =
      decoder(index, row)
  }
  implicit val stringDecoder: Decoder[String] = decoder[String]
  implicit val intDecoder: Decoder[Int] = decoder[Int]

  def decoder[T: ClassTag]: Decoder[T] = MirrorDecoder((index: Int, row: ResultRow) => row[T](index))
  

  object Decoder {
  inline def summonAndDecode[T](index: Int, resultRow: ResultRow): T =
    summonFrom {
        case dec: Decoder[T] => dec(index, resultRow)
    }

  inline def arity[Elems <: Tuple]: Int =
    inline erasedValue[Elems] match {
      case _: (head *: tail) => 1 + arity[tail]
      case _ => 0
    }

  inline def tuplizeChildren[Elems <: Tuple](index: Int, resultRow: ResultRow): Tuple =
    inline erasedValue[Elems] match {
      case _: (Product *: tail) =>
        val (air, output) =
          inline erasedValue[Elems] match { 
            case _: (head *: tail) =>
              val ret = summonAndDecode[head](index, resultRow)
              val air = ret.asInstanceOf[Product].productArity
              (air, ret)
          }
        (output *: tuplizeChildren[tail](index + air, resultRow)) 
      case b: (head *: tail) =>
        (summonAndDecode[head](index, resultRow) *: tuplizeChildren[tail](index + 1, resultRow))
      case _ => ()
    }

  inline def derived[T](given ev: Mirror.Of[T]): Decoder[T] = new Decoder[T]() {
    def apply(index: Int, resultRow: ResultRow): T =
      inline ev match {
        case m: Mirror.ProductOf[T] =>
          val tup = tuplizeChildren[m.MirroredElemTypes](index, resultRow)
          m.fromProduct(tup.asInstanceOf[Product]).asInstanceOf[T]
      }
  }
  }
}


// TODO If want to have Index by a type, need some way to add to it
trait CoreDsl extends EncodingDsl

trait EncodingDsl {
  type PrepareRow
  type ResultRow = Row
  //type Index = Int
  type BaseEncoder[T] = (Int, T, PrepareRow) => PrepareRow
  type Encoder[T] <: BaseEncoder[T]

  //type BaseDecoder[T] = (Int, ResultRow) => T
  //type Decoder[T]  = (Int, ResultRow) => T
  trait Decoder[T] {
    def apply(i: Int, rr: ResultRow):T
  }
}

trait MetaDsl { this: CoreDsl =>
  trait QueryMeta[T] {
    def expand: Query[T] => Query[_] //Quoted[Query[T] => Query[_]]
    def extract: ResultRow => T
  }
}

case class Row(data: Any*) {
  def add(value: Any) = Row((data :+ value): _*)
  def apply[T](index: Int)(given t: ClassTag[T]) =
    data(index) match {
      case v: T  => v
      case other => throw new RuntimeException(s"Invalid column type. Expected '${t.runtimeClass}', but got '$other'")
    }
}