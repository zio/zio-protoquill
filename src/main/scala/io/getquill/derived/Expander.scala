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
        val childTerm = Term(field.constValue, optional = true)
        base(childTerm)(given tpe) :: flatten(node, fields, types)

      case ('[$field *: $fields], '[$tpe *: $types]) if (tpe.isProduct) =>
        val childTerm = Term(field.constValue)
        base(childTerm)(given tpe) :: flatten(node, fields, types)

      case ('[$field *: $fields], '[Option[$tpe] *: $types]) =>
        val childTerm = Term(field.constValue, optional = true)
        childTerm :: flatten(node, fields, types)

      case ('[$field *: $fields], '[$tpe *: $types]) =>
        val childTerm = Term(field.constValue)
        childTerm :: flatten(node, fields, types)

      case (_, '[Unit]) => Nil
    } 
  }

  def base[T](term: Term)(given tpe: Type[T])(given qctx: QuoteContext): Term = {
    import qctx.tasty.{Type => QType, Term => QTerm, _, given}

    // if there is a decoder for the term, just return the term
    summonExpr(given '[GenericDecoder[_, T]]) match {
      case Some(decoder) => term
      case None => {
        // Otherwise, recursively summon fields
        val ev: Expr[Mirror.Of[T]] = summonExpr(given '[Mirror.Of[$tpe]]).get
        ev match {
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = $elementLabels; type MirroredElemTypes = $elementTypes }} =>
            val children = flatten(term, elementLabels, elementTypes)
            term.withChildren(children) 
        }
      }
    }
  }

  import io.getquill.ast.{Map => AMap, _}
  def static[T](ast: Ast)(given qctx: QuoteContext, tpe: Type[T]): AMap = {
    val expanded = base[T](Term("x"))(given tpe)
    AMap(ast, Ident("x"), Tuple(expanded.toAst))    
  }

  import miniquill.parser.Lifter
  // TODO Load from implicit context?
  def lifterFactory: (QuoteContext) => PartialFunction[Ast, Expr[Ast]] =
    (qctx: QuoteContext) => new Lifter(given qctx)  

  inline def runtime[T](ast: Ast): AMap = ${ runtimeImpl[T]('ast) }
  def runtimeImpl[T](ast: Expr[Ast])(given qctx: QuoteContext, tpe: Type[T]): Expr[AMap] = {
    val expanded = base[T](Term("x"))(given tpe)
    val lifted = expanded.toAst.map(ast => lifterFactory(qctx).apply(ast))
    // implicit def astLiftable: Liftable[Ast] = new Liftable {
    //   def toExpr(ast: Ast) = lifterFactory(qctx).apply(ast)
    // }
    //val liftedTerms = expanded.toAst.map(term => lifterFactory(qctx).apply(term))
    //val exprLiftedTerms = ExprSeq(liftedTerms)
    '{ AMap($ast, Ident("x"), Tuple(${Expr.ofList(lifted)})) }
  }
}
