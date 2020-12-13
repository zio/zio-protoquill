package io.getquill.derived

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._


object Expander {
  import miniquill.dsl.GenericDecoder

  sealed trait TermType
  case object Leaf extends TermType
  case object Branch extends TermType

  case class Term(name: String, typeType: TermType, children: List[Term] = List(), optional: Boolean = false) {
    def withChildren(children: List[Term]) = this.copy(children = children)
    def toAst = Term.toAst(this)
  }
  object Term {
    import io.getquill.ast._

    // Not sure if Branch Terms should have hidden properties
    def property(parent: Ast, name: String, termType: TermType) =
      Property.Opinionated(parent, name, Renameable.neutral, 
        termType match {
          case Leaf => Visible
          case Branch => Hidden
        }
      )

    def toAst(node: Term, parent: Ast): List[Ast] =
      node match {
        // If there are no children, property should not be hidden i.e. since it's not 'intermediate'
        // I.e. for the sake of SQL we set properties representing embedded objects to 'hidden'
        // so they don't show up in the SQL.
        case Term(name, tt, Nil, _) =>
          List(property(parent, name, tt))

        case Term(name, tt, list, false) =>
          list.flatMap(elem => 
            toAst(elem, property(parent, name, tt)))
          
        case Term(name, Leaf, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield OptionMap(property(parent, name, Leaf), idV, newAst)

        case Term(name, Branch, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield OptionTableMap(property(parent, name, Branch), idV, newAst)
      }

    def toAst(node: Term): List[Ast] = {
      node match {
        // If leaf node, return the term, don't care about if it is optional or not
        case Term(name, _, Nil, _) =>
          List(Ident(name))

        // T( a, [T(b), T(c)] ) => [ a.b, a.c ] 
        // (done?)         => [ P(a, b), P(a, c) ] 
        // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
        // where T is Term and P is Property (in Ast) and [] is a list
        case Term(name, _, list, false) =>
          list.flatMap(elem => toAst(elem, Ident(name)))

        // T-Opt( a, T(b), T(c) ) => 
        // [ a.map(v => v.b), a.map(v => v.c) ] 
        // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
        // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]
        case Term(name, _, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield Map(Ident(name), idV, newAst)
      }
    }
  }

  class TypeExtensions(using Quotes) { self =>
    import quotes.reflect._
    
    implicit class TypeExt(tpe: Type[_]) {
      def constValue = self.constValue(tpe)
      def isProduct = self.isProduct(tpe)
    }

    def constValue(tpe: Type[_]): String =
      TypeRepr.of(using tpe) match {
        case ConstantType(Constant.Int(value)) => value.toString
        case ConstantType(Constant.String(value)) => value.toString
        // Macro error
      }
    def isProduct(tpe: Type[_]): Boolean =
      TypeRepr.of(using tpe) <:< TypeRepr.of[Product]
  }

  def flatten[Fields, Types](node: Term, fieldsTup: Type[Fields], typesTup: Type[Types])(using Quotes): List[Term] = {
    import quotes.reflect.{Term => QTerm, _}
    val ext = new TypeExtensions
    import ext._

    def constValue[T: Type]: String =
      TypeRepr.of[T] match {
        case ConstantType(Constant.Int(value)) => value.toString
        case ConstantType(Constant.String(value)) => value.toString
        // Macro error
      }

    (fieldsTup, typesTup) match {
      // TODO These calls are expensive
      // do this first '[field *: fields], then do '[Option[tpe] *: types] internally

      case ('[field *: fields], '[Option[tpe] *: types]) if Type.of[tpe].isProduct =>
        val childTerm = Term(Type.of[field].constValue, Branch, optional = true)
        base[tpe](childTerm) :: flatten(node, Type.of[fields], Type.of[types])

      case ('[field *: fields], '[tpe *: types]) if Type.of[tpe].isProduct =>
        val childTerm = Term(Type.of[field].constValue, Branch)
        base[tpe](childTerm) :: flatten(node, Type.of[fields], Type.of[types])

      case ('[field *: fields], '[Option[tpe] *: types]) =>
        val childTerm = Term(Type.of[field].constValue, Leaf, optional = true)
        childTerm :: flatten(node, Type.of[fields], Type.of[types])

      case ('[field *: fields], '[tpe *: types]) =>
        val childTerm = Term(Type.of[field].constValue, Leaf)
        childTerm :: flatten(node, Type.of[fields], Type.of[types])

      case (_, '[EmptyTuple]) => Nil

      case _ => report.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
    } 
  }

  def base[T: Type](term: Term)(using Quotes): Term = {
    import quotes.reflect.{Term => QTerm, _}

    // if there is a decoder for the term, just return the term
    Expr.summon[Mirror.Of[T]] match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val children = flatten(term, Type.of[elementLabels], Type.of[elementTypes])
            term.withChildren(children)
          case _ =>
            Expr.summon[GenericDecoder[_, T]] match {
              case Some(decoder) => term
              case _ => report.throwError("Cannot Find Decoder or Expand a Product of the Type:\n" + ev.show)
            }
        }
      }
      case None => 
        Expr.summon[GenericDecoder[_, T]] match {
          case Some(decoder) => term
          case None => report.throwError(s"Cannot find derive or summon a decoder for ${Type.show[T]}")
        }
    }
  }

  import io.getquill.ast.{Map => AMap, _}
  def static[T](ast: Ast)(using Quotes, Type[T]): AMap = {
    val expanded = base[T](Term("x", Branch))
    val lifted = expanded.toAst
    //println("Expanded to: " + expanded)
    val insert =
      if (lifted.length == 1)
        lifted.head
      else
        Tuple(lifted)

    AMap(ast, Ident("x"), insert)
  }

  import miniquill.parser.Lifter
  // TODO Load from implicit context?
  def lifterFactory: (Quotes) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: Quotes) => new Lifter(using qctx)  

  inline def runtime[T](ast: Ast): AMap = ${ runtimeImpl[T]('ast) }
  def runtimeImpl[T](ast: Expr[Ast])(using Quotes, Type[T]): Expr[AMap] = {
    val expanded = base[T](Term("x", Branch))
    val lifted = expanded.toAst.map(ast => lifterFactory(quotes).apply(ast))
    val insert = 
      if (lifted.length == 1) 
        lifted.head 
      else 
        '{ Tuple(${Expr.ofList(lifted)}) }

    '{ AMap($ast, Ident("x"), $insert) }
  }

  def runtimeImplTest[T](ast: Expr[Ast])(using Quotes, Type[T]): Expr[AMap] = {
    '{ AMap($ast, ???, ???) }
  }
}
