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

  // TODO Define this way for any scalar
  //given Expander[String] = new Expander[String] { def expand(inject: Ast) = List(inject) }

  // can't expand 'Map' from the base this way since Map needs to know field of the base object
  // Define an expander for Option. Specifically, Option has special treatment in the AST generation.
  //given mapExpander[T](given Expander[T]): Expander[Option[T]] = Expander.derived

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
    inline erasedValue[(Fields, Types)] match {
       // TODO The non-product type variation of this works like a recular scalar?

       // If fields of the record are inside Option, expand them based on Option.map(v => ...)
       // The 'v' member will now become the base of the inner-expansion of the tuple.
      case (_: (field *: fields), _:(Option[ProductType[tpe]] *: types)) =>
        val fieldName = constValue[field].toString
        val idV = Ident("v")
        val nestedFields = nestedExpand[tpe](idV)
        nestedFields.map(nestedField => AMap(Property(inject, fieldName), idV, nestedField))

      // Our record contains products in any fields, expand the sub fields from this property onward
      // For example where:
      //   Inject := x,  Outer(a, b: Inner), Inner(i, j) and a,i,j are Scalars
      // Say we are traversing through the fields `a` and `b` of Outer
      // The following expressions will be seen. The 'i' and 'j' elements are future expasions of this function.
      //   => (x.a), (x.b).i, (x.b).j
      case (_: (field *: fields), _:(ProductType[tpe] *: types)) =>
        val fieldName = constValue[field].toString
        val nextInject = Property(inject, fieldName)
        val nestedFields = nestedExpand[tpe](nextInject)
        nestedFields ++ flatten[fields, types](inject)
      
      // If it's a scalar, just expand the current value
      case (_: (field *: fields), _: (tpe *: types)) =>
        val fieldName = constValue[field].toString
        Property(inject, fieldName) :: flatten[fields, types](inject)
      

      case _ => Nil
    }
  }


  // TODO What if the outermost element is an option? Need to test that with the original Quill MetaDslSpec.
  inline def derived[T](given m: Mirror.Of[T]): Expander[T] = new Expander[T] {
    def expand(inject: Ast) =
      inline m match {
        // Special treatment for option
        case pm: Mirror.ProductOf[T] => flatten[pm.MirroredElemLabels, pm.MirroredElemTypes](inject)
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
