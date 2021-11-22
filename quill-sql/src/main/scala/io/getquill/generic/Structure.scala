package io.getquill.generic

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import io.getquill.ast.Visibility.{ Hidden, Visible }
import scala.deriving._
import scala.quoted._
import io.getquill.parser.Lifter
import io.getquill.quat.Quat
import io.getquill.ast.{Map => AMap, _}
import io.getquill.metaprog.TypeExtensions
import io.getquill.metaprog.TypeExtensions._
import io.getquill.generic.DecodingType
import io.getquill.util.Format
import scala.annotation.tailrec

sealed trait Structure:
  def name: String
  def optional: Boolean
  def children: List[Structure]
  def asLeaf: Structure
  def withChildren(children: List[Structure]): Structure.Node
  def toAst = Structure.toAstTop(this)
  def isLeaf: Boolean
  def structType: Structure.StructType
  // Used by coproducts, merges all fields of a term with another if this is valid
  // Note that T is only needed for the error message. Maybe take it out once we store Types inside of Term
  def merge[T: Type](other: Structure)(using quotes: Quotes) =
    import quotes.reflect.{ Term => TTerm, _ }

    // Terms must both have the same name
    if (this.name != other.name)
      report.throwError(s"Cannot resolve coproducts because terms ${this} and ${other} have different names") // TODO Describe this as better error messages for users?

    if (this.optional != other.optional)
      report.throwError(s"Cannot resolve coproducts because one of the terms ${this} and ${other} is optional and the other is not")

    (this, other) match
      case (self: Structure.Node, _: Structure.Node) =>
        import io.getquill.util.GroupByOps._
        // Given Shape -> (Square, Rectangle) the result will be:
        // Shape.x detected multiple kinds of values: List(Term(x,Branch,List(Term(width,Leaf,List(),false), Term(height,Leaf,List(),false)),false), Term(x,Branch,List(Term(radius,Leaf,List(),false)),false))
        // Need to merge these terms
        val orderedGroupBy = (self.children ++ other.children).groupByOrdered(_.name)
        // Validate the keys to make sure that they are all the same kind of thing. E.g. if you have a Shape coproduct
        // with a Square.height and a Rectagnle.height, both 'height' fields but be a Leaf (and also in the future will need to have the same data type)
        // TODO Need to add datatype to Term so we can also verify types are the same for the coproducts
        val newChildren =
          orderedGroupBy.map((term, values) => {
            val distinctValues = values.distinct
            if (distinctValues.length > 1)
              report.throwError(s"Invalid coproduct at: ${TypeRepr.of[T].widen.typeSymbol.name}.${term} detected multiple kinds of values: ${distinctValues}")

            // TODO Check if there are zero?
            distinctValues.head
          }).toList
        self.copy(children = newChildren)

      case (self: Structure.Leaf, _: Structure.Leaf) =>
        self

      case (a, b) =>
        report.throwError(s"Cannot resolve coproducts because the terms ${this} and ${other} have different types (${a.structType} and ${b.structType} respectively)")

  end merge

  def paths =
    def pathsRecurse(node: Structure, topLevel: Boolean = false): List[String] =
      def emptyIfTop(str: String) = if(topLevel) "" else str
      node match
        case Structure.Leaf(name, _) => List(name)
        case Structure.Node(name, _, childProps) =>
          childProps
            .flatMap(childTerm => pathsRecurse(childTerm))
            .map(childName => emptyIfTop(name) + childName)
    pathsRecurse(this, true)
  end paths
end Structure

object Structure:

  // TODO Good use-case for zio-chunk
  case class TermPath(terms: List[Structure]):
    def append(term: Structure) = this.copy(terms = this.terms :+ term)
    def concat(path: TermPath) = this.copy(terms = this.terms ++ path.terms)
    def mkString(separator: String = "", dropFirst: Boolean = true) =
      (if (dropFirst) terms.drop(1) else terms).map(_.name).mkString(separator)
  object TermPath:
    def single(term: Structure) = TermPath(List(term))

  case class Leaf(name: String, optional: Boolean = false) extends Structure:
    def withChildren(children: List[Structure]): Node = Structure.Node(name, optional, children)
    def children = List()
    def asLeaf = this
    def isLeaf = true
    def structType = StructType.Leaf

  case class Node(name: String, optional: Boolean = false, children: List[Structure] = List()) extends Structure:
    def withChildren(children: List[Structure]): Node = this.copy(children = children)
    def asLeaf = Leaf(this.name, this.optional)
    def isLeaf = false
    def structType = StructType.Node


  import io.getquill.ast._

  enum StructType:
    case Leaf
    case Node

  // Not sure if Branch Terms should have hidden properties
  private def property(parent: Ast, name: String, termType: StructType) =
    Property.Opinionated(parent, name, Renameable.neutral,
      termType match
        case StructType.Leaf => Visible
        case StructType.Node => Hidden
    )

  def toAst(node: Structure, parent: Ast): List[(Ast, String)] =
    node match {
      // If there are no children, property should not be hidden i.e. since it's not 'intermediate'
      // I.e. for the sake of SQL we set properties representing embedded objects to 'hidden'
      // so they don't show up in the SQL.
      case Structure.Leaf(name, _) =>
        val output = List(property(parent, name, StructType.Leaf))
        // Add field name to the output: x.width -> (x.width, width)
        output.map(ast => (ast, name))

      case Structure.Node(name, false, list) =>
        val output =
          list.flatMap(elem =>
            toAst(elem, property(parent, name, StructType.Node)))
        // Add field name to the output
        output.map((ast, childName) => (ast, name + childName))

      // Product node in a option
      case Structure.Node(name, true, list) =>
        val idV = Ident("v", Quat.Generic)
        val output =
          for {
            elem <- list
            (newAst, subName) <- toAst(elem, idV)
          } yield (OptionTableMap(property(parent, name, StructType.Node), idV, newAst), subName)

        // Add field name to the output
        output.map((ast, childName) => (ast, name + childName))
    }

  /**
   * Top-Level expansion of a Term is slighly different the later levels. A the top it's always ident.map(id => ...)
   * if Ident is an option as opposed to OptionMap which it would be, in lower layers.
   *
   * Legend:
   *   T(x, [y,z])      := Term(x=name, children=List(y,z)), T-Opt=OptionalTerm I.e. term where optional=true
   *   P(a, b)          := Property(a, b) i.e. a.b
   *   M(a, v, P(v, b)) := Map(a, v, P(v, b)) or m.map(v => P(v, b))
   */
  private def toAstTop(node: Structure): List[(Ast, String)] = {
    node match {
      // Node without children
      // If leaf node, return the term, don't care about if it is optional or not
      case Structure.Leaf(name, _) =>
        val output = List(Ident(name, Quat.Generic))
        // For a single-element field, add field name to the output
        output.map(ast => (ast, name))

      // Product node not inside an option.
      // T( a, [T(b), T(c)] ) => [ a.b, a.c ]
      // (done?)         => [ P(a, b), P(a, c) ]
      // (recurse more?) => [ P(P(a, (...)), b), P(P(a, (...)), c) ]
      // where T is Term and P is Property (in Ast) and [] is a list
      case Structure.Node(name, false, list) =>
        val output = list.flatMap(elem => toAst(elem, Ident(name, Quat.Generic)))
        // Do not add top level field to the output. Otherwise it would be x -> (x.width, xwidth), (x.height, xheight)
        output

      // Production node inside an Option
      // T-Opt( a, [T(b), T(c)] ) =>
      // [ a.map(v => v.b), a.map(v => v.c) ]
      // (done?)         => [ M( a, v, P(v, b)), M( a, v, P(v, c)) ]
      // (recurse more?) => [ M( P(a, (...)), v, P(v, b)), M( P(a, (...)), v, P(v, c)) ]
      case Structure.Node(name, true, list) =>
        val idV = Ident("v", Quat.Generic)
        val output =
          for {
            elem <- list
            (newAst, name) <- toAst(elem, idV)
            // TODO Is this right? Should it be OptionTableMap?
          } yield (Map(Ident(name, Quat.Generic), idV, newAst), name)
        // Do not add top level field to the output. Otherwise it would be x -> (x.width, xwidth), (x.height, xheight)
        output
    }
  }
end Structure
