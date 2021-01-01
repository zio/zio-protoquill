package io.getquill.derived

import io.getquill.quoter._
import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.parser.Lifter

/**
 * This was around to flesh-out details of the outermost AST of a query based on the fields of the
 * object T in Query[T] that the AST represents. For an example say we have something like this:
 * {{{
 * import io.getquill.ast.{ Ident => Id, Property => Prop, _ }
 * case class Person(name: String, age: Int)
 * query[Person].map(p => p) // or just query[Person]
 * }}}
 * That would turn into an AST that looks like this:
 * {{{
 * Map(EntityQuery("Person"), Id("p"), Id("p"))
 * }}}
 * This query needs to be turned into `SELECT p.name, p.age from Person p`, the problem is, before
 * Quats, Quill did not actually know how to expand `Ident("p")` into `SelectValue(p.name), SelectValue(p.age)`
 * (see SqlQuery.scala) since there was no type information. Therefore...
 * {{{
 * // We needed to convert something that looks like this:
 * query[Person].map(p => p) // i.e. Map(EntityQuery("Person"), Id("p"), Id("p"))
 * 
 * // Into something that looks like this:
 * query[Person].map(p => p).map(p => (p.name, p.age)) 
 * // i.e. Map(Map(EntityQuery("Person"), Ident("p"), Ident("p")), Tuple(Prop(Id("p"),"name"), Prop(Id("p"),"age")))
 * }}}
 * This makes it easier to translate the above information into the finalized form
 * {{{
 * SELECT p.name, p.age FROM (SELECT p.* from Person p) AS p
 * }}}
 * (Note that redudant map would typically be flattened out since it is extraneous and the inner 
 * SELECT would no longer be present)
 *
 * Some special provisions were made for fields inside optional objects:
 * {{{
 * case class Address(street: String, zip: Int)
 * case class Person(name: String, address: Option[Address])
 * // This:
 * query[Person]
 * // Would become this:
 * query[Person].map(p => (p.name, p.address.map(_.street), p.address.map(_.zip)))
 * }}}
 * 
 * Now, since Quats were introduced into Quill since 3.6.0 (technically since 3.5.3), this step is not necessarily needed
 * for query expansion since `Ident("p")` is now understood to expand into it's corresponding SelectValue fields so for queries,
 * this stage could technically be elimiated. However, this logic is also useful for ActionMeta where we have
 * something like this:
 * {{{
 * case class Person(name: String, age: Int)
 * // This:
 * query[Person].insert(Person("Joe", 44))
 * // Needs to be converted into this:
 * query[Person].insert(_.name -> "Joe", _.age -> 44)
 * // Which is actually:
 * EntityQuery("Person").insert(
 *   Assignment(Id("x1"), Prop(Id("x1"), "name"), Const("Joe")), 
 *   Assignment(Id("x1"), Prop(Id("x1"), "name"), Const(44))
 * )
 * }}}
 * The fact that we know that Person expands into Prop(Id("p"),"name"), Prop(Id("p"),"age")) helps
 * us compute the necessary assignments in the `InsertMacro`.
 */
object ElaborateQueryMeta {
  import io.getquill.dsl.GenericDecoder

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
        case ConstantType(IntConstant(value)) => value.toString
        case ConstantType(StringConstant(value)) => value.toString
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
        case ConstantType(IntConstant(value)) => value.toString
        case ConstantType(StringConstant(value)) => value.toString
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

  def staticList[T](baseName: String)(using Quotes, Type[T]): List[Ast] = {
    val expanded = base[T](Term(baseName, Branch))
    expanded.toAst
  }
  
  /** ElaborateQueryMeta the query AST in a static query **/
  def static[T](ast: Ast)(using Quotes, Type[T]): AMap = {
    val lifted = staticList[T]("x")
    //println("Expanded to: " + expanded)
    val insert =
      if (lifted.length == 1)
        lifted.head
      else
        Tuple(lifted)

    AMap(ast, Ident("x"), insert)
  }

  /** An external hook to run the Elaboration with a given AST during runtime (mostly for testing). */
  inline def external[T](ast: Ast): AMap = ${ dynamic[T]('ast) }

  /** ElaborateQueryMeta the query AST in a dynamic query **/
  def dynamic[T](ast: Expr[Ast])(using Quotes, Type[T]): Expr[AMap] = {
    val lifted = staticList[T]("x").map(ast => Lifter(ast))
    val insert = 
      if (lifted.length == 1) 
        lifted.head 
      else 
        '{ Tuple(${Expr.ofList(lifted)}) }

    '{ AMap($ast, Ident("x"), $insert) }
  }
}
