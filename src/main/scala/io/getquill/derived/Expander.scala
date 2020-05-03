package io.getquill.derived

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import scala.quoted.matching._

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
          
        case Term(name, tt, list, true) =>
          val idV = Ident("v")
          for {
            elem <- list
            newAst <- toAst(elem, idV)
          } yield Map(
            property(parent, name, tt), idV, newAst
          )
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

  class TypeExtensions(given qctx: QuoteContext) { self =>
    import qctx.tasty.{Type => QType, given, _}
    
    implicit class TypeExt(tpe: Type[_]) {
      def constValue = self.constValue(tpe)
      def isProduct = self.isProduct(tpe)
    }

    def constValue(tpe: Type[_]): String =
      tpe.unseal.tpe match {
        case ConstantType(Constant(value)) => value.toString
        // Macro error
      }
    def isProduct(tpe: Type[_]): Boolean =
      tpe.unseal.tpe <:< '[Product].unseal.tpe
  }

  def flatten[Fields, Types](node: Term, fieldsTup: Type[Fields], typesTup: Type[Types])(given qctx: QuoteContext): List[Term] = {
    import qctx.tasty.{Type => QType, Term => QTerm, given, _}
    val ext = new TypeExtensions
    import ext._

    def constValue[T](tpe: Type[T]): String =
      tpe.unseal.tpe match {
        case ConstantType(Constant(value)) => value.toString
        // Macro error
      }

    (fieldsTup, typesTup) match {
      case ('[$field *: $fields], '[Option[$tpe] *: $types]) if (tpe.isProduct) =>
        val childTerm = Term(field.constValue, Branch, optional = true)
        base(childTerm)(given tpe) :: flatten(node, fields, types)

      case ('[$field *: $fields], '[$tpe *: $types]) if (tpe.isProduct) =>
        val childTerm = Term(field.constValue, Branch)
        base(childTerm)(given tpe) :: flatten(node, fields, types)

      case ('[$field *: $fields], '[Option[$tpe] *: $types]) =>
        val childTerm = Term(field.constValue, Leaf, optional = true)
        childTerm :: flatten(node, fields, types)

      case ('[$field *: $fields], '[$tpe *: $types]) =>
        val childTerm = Term(field.constValue, Leaf)
        childTerm :: flatten(node, fields, types)

      case (_, '[Unit]) => Nil

      case _ => qctx.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
    } 
  }

  def base[T](term: Term)(given tpe: Type[T])(given qctx: QuoteContext): Term = {
    import qctx.tasty.{Type => QType, Term => QTerm, _, given}

    // if there is a decoder for the term, just return the term
    summonExpr(given '[Mirror.Of[$tpe]]) match {
      case Some(ev) => {
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $elementLabels; type MirroredElemTypes = $elementTypes }} =>
            val children = flatten(term, elementLabels, elementTypes)
            term.withChildren(children)
          case _ =>
            summonExpr(given '[GenericDecoder[_, T]]) match {
              case Some(decoder) => term
              case _ => qctx.throwError("Cannot Find Decoder or Expand a Product of the Type:\n" + ev.show)
            }
        }
      }
      case None => 
        summonExpr(given '[GenericDecoder[_, T]]) match {
          case Some(decoder) => term
          // None => TODO Macro Error, cannot find decoder for tail entity (i.e. decoder not found and it's not a product)
        }
    }
  }

  import io.getquill.ast.{Map => AMap, _}
  def static[T](ast: Ast)(given qctx: QuoteContext, tpe: Type[T]): AMap = {
    val expanded = base[T](Term("x", Branch))(given tpe)
    val lifted = expanded.toAst
    println("Expanded to: " + expanded)
    val insert =
      if (lifted.length == 1)
        lifted.head
      else
        Tuple(lifted)

    AMap(ast, Ident("x"), insert)
  }

  import miniquill.parser.Lifter
  // TODO Load from implicit context?
  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)  

  inline def runtime[T](ast: Ast): AMap = ${ runtimeImpl[T]('ast) }
  def runtimeImpl[T](ast: Expr[Ast])(given qctx: QuoteContext, tpe: Type[T]): Expr[AMap] = {
    val expanded = base[T](Term("x", Branch))(given tpe)
    val lifted = expanded.toAst.map(ast => lifterFactory(qctx).apply(ast))
    val insert = 
      if (lifted.length == 1) 
        lifted.head 
      else 
        '{ Tuple(${Expr.ofList(lifted)}) }

    '{ AMap($ast, Ident("x"), $insert) }
  }
}
