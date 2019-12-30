package miniquill.meta

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._

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


trait CoreDsl extends EncodingDsl

trait EncodingDsl {
  type PrepareRow
  type ResultRow
  type Index = Int
  type BaseEncoder[T] = (Index, T, PrepareRow) => PrepareRow
  type Encoder[T] <: BaseEncoder[T]
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