package miniquill.dsl

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue

// trait Expander[T] {
//   def expand: Ast
// }

trait Expander[T] {
  def expand(inject: Ast): List[Ast]
}

object Expander {

  // given optionExpander: Expand[Option[T]](given exp: Expander[T]) = new Expander[Option[T]] {
  //   def expand(inject: Ast) =
  //     AMap(inject, )
  // }

  type ProductType[T <: Product] = T

  inline def nestedExpand[T](inject: Ast): List[Ast] =
    summonFrom {
      case exp: Expander[T] => exp.expand(inject)
    }

  inline def flatten[Fields <: Tuple, Types <: Tuple](inject: Ast): List[Ast] = {
    inline (erasedValue[Fields], erasedValue[Types]) match {
      // if it contains sub-fields, expand the sub fields from this property onward
      // for example where Inject is x,  Outer(a, b: Inner), where Inner(i, j) 
      //   => (x.a), (x.b).i, (x.b).j   - The expressions in commas are the next thing that will be injected
      case _: ((field *: fields), (ProductType[tpe] *: types)) => {
        val fieldName = constValue[field].toString
        val nextInject = Property(inject, fieldName)
        val nestedFields = nestedExpand[tpe](nextInject)
        nestedFields ++ flatten[fields, types](inject)
      }
      // If it's a scalar, just expand the current value
      case _: ((field *: fields), (tpe *: types)) => {
        val fieldName = constValue[field].toString
        Property(inject, fieldName) :: flatten[fields, types](inject)
      }

      case _ => Nil
    }
  }

  inline def derived[T](given m: Mirror.Of[T]): Expander[T] = new Expander[T] {
    def expand(inject: Ast) =
      inline m match {
        case pm: Mirror.ProductOf[T] => null
      }
  }


}




// TODO If want to have Index by a type, need some way to add to it
trait CoreDsl extends EncodingDsl



trait MetaDsl { this: CoreDsl =>
  trait QueryMeta[T] {
    def expand: Query[T] => Query[_] //Quoted[Query[T] => Query[_]]
    def extract: ResultRow => T
  }
}
