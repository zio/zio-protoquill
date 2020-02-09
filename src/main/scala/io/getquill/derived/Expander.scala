package io.getquill.derived

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }

trait Expander[T] {
  import Expander._

  def expand(node: Term): Term
  def expandAst(ast: Ast): Ast // TODO For leaf nodes this should be a no-op
}

object Expander {
  import miniquill.dsl.GenericDecoder

  // If there is a decoder for it, it's a leaf node
  given decodeableExpander[T](using GenericDecoder[_, T]) as Expander[T] =
    new Expander[T] {
      def expand(node: Term): Term = node
      def expandAst(ast: Ast): Ast  = ast
    }

  case class Term(name: String, children: List[Term] = List(), optional: Boolean = false) {
    def withChildren(children: List[Term]) = this.copy(children = children)
    def toAst = Term.toAst(this)
  }
  object Term {
    import io.getquill.ast._

    def toAst(node: Term, parent: Ast): List[Ast] =
      node match {
        // If there are no children, property should not be hidden i.e. since it's not 'intermediate'
        // I.e. for the sake of SQL we set properties representing embedded objects to 'hidden'
        // so they don't show up in the SQL.
        case Term(name, Nil, _) =>
          List(Property.Opinionated(parent, name, Renameable.neutral, Visible))

        case Term(name, list, false) =>
          list.flatMap(elem => 
            toAst(elem, Property.Opinionated(parent, name, Renameable.neutral, Hidden)))
          
        case Term(name, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield Map(
            Property.Opinionated(parent, name, Renameable.neutral, Hidden), idV, newAst
          )
      }

    def toAst(node: Term): List[Ast] = {
      node match {
        // If leaf node, return the term, don't care about if it is optional or not
        case Term(name, Nil, _) =>
          List(Ident(name))

        // T( a, [T(b), T(c)] ) => [ a.b, a.c ] 
        // (done?)         => [ P(a, b), P(a, c) ] 
        // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
        // where T is Term and P is Property (in Ast) and [] is a list
        case Term(name, list, false) =>
          list.flatMap(elem => toAst(elem, Ident(name)))

        // T-Opt( a, T(b), T(c) ) => 
        // [ a.map(v => v.b), a.map(v => v.c) ] 
        // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
        // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]
        case Term(name, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield Map(Ident(name), idV, newAst)
      }
    }
  }



  type ProductType[T <: Product] = T

  inline def nestedExpand[T](node: Term): Term =
    summonFrom {
      case exp: Expander[T] => exp.expand(node)
    }

  // TODO Do we need tail recursion? If so, add an accumulator list and only return the final product in the Nil case
  inline def flatten[Fields <: Tuple, Types <: Tuple](node: Term): List[Term] = {
    inline erasedValue[(Fields, Types)] match {

      case (_: (field *: fields), _:(Option[ProductType[tpe]] *: types)) =>
        val childTerm = Term(constValue[field].toString, optional = true)
        nestedExpand[tpe](childTerm) :: flatten[fields, types](node)

      case (_: (field *: fields), _:(ProductType[tpe] *: types)) =>
        val childTerm = Term(constValue[field].toString)
        nestedExpand[tpe](childTerm) :: flatten[fields, types](node)
      
      // Typically we don't care that leaf elements are optional but write the info anyway
      case (_: (field *: fields), _: (Option[tpe] *: types)) =>
        val childTerm = Term(constValue[field].toString, optional = true)
        childTerm :: flatten[fields, types](node)

      case (_: (field *: fields), _: (tpe *: types)) =>
        val childTerm = Term(constValue[field].toString)
        childTerm :: flatten[fields, types](node)

      case _ => Nil
    }
  }


  // TODO What if the outermost element is an option? Need to test that with the original Quill MetaDslSpec.
  inline def derived[T]: Expander[T] = new Expander[T] {
    def expand(node: Term): Term =
      summonFrom {
        case ev: Mirror.Of[T] =>
          inline ev match {
            // TODO What if root-term is optional
            // TODO Special treatment for option
            case pm: Mirror.ProductOf[T] => 
              val children = flatten[pm.MirroredElemLabels, pm.MirroredElemTypes](node)
              node.withChildren(children)
          }
      }

    def expandAst(ast: Ast): Ast = {
      import io.getquill.ast.{Map => AMap, _}
      import io.getquill.norm.BetaReduction

      val expanded = expand(Term("x"))
      

      AMap(ast, Ident("x"), Tuple(expanded.toAst))
    }
  }

  

}
