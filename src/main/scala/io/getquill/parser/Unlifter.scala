package io.getquill.parser

import scala.quoted._
import scala.reflect.ClassTag
import scala.reflect.classTag;

import io.getquill.ast.{Ident => AIdent, Query => Qry, _}

object UnlifterType {
  type Unlift[T] = PartialFunction[Expr[T], T]
}

// TODO Change to same way of doing things as Lifter, then shuold be able to get rid of lift and tuple2 unlifter
object Unlifter {
  import UnlifterType._

  def apply(ast: Expr[Ast]): Quotes ?=> Ast = unliftAst.apply(ast) // can also do ast.lift but this makes some error messages simpler

  extension [T](t: Expr[T])(using FromExpr[T], Quotes)
    def unexpr: T = t.valueOrError

  // def apply(astExpr: Expr[Ast]): Ast = astExpr.unexpr
  // def isDefinedAt(astExpr: Expr[Ast]): Boolean = unliftAst.isDefinedAt(astExpr)
  // private def constString(expr: Expr[Any]): String = 
  //   expr match { case Const(v: String) => v }

  // implicit def unliftString: Unlift[String] = { case Const(v: String) => v }
  // implicit def unliftDouble: Unlift[Double] = { case Const(v: Double) => v }
  // implicit def unliftFloat: Unlift[Float] = { case Const(v: Float) => v }
  // implicit def unliftLong: Unlift[Long] = { case Const(v: Long) => v }
  // implicit def unliftInt: Unlift[Int] = { case Const(v: Int) => v }
  // implicit def unliftShort: Unlift[Short] = { case Const(v: Short) => v }
  // implicit def unliftByte: Unlift[Byte] = { case Const(v: Byte) => v }
  // implicit def unliftChar: Unlift[Char] = { case Const(v: Char) => v }
  // implicit def unliftBoolean: Unlift[Boolean] = { case Const(v: Boolean) => v }

  trait NiceUnliftable[T: ClassTag] extends FromExpr[T] { // : ClassTag
    def unlift: Quotes ?=> PartialFunction[Expr[T], T]
    def apply(expr: Expr[T])(using Quotes): T =
      unlift.lift(expr).getOrElse { throw new IllegalArgumentException(s"Could not Unlift AST type ${classTag[T].runtimeClass.getSimpleName} from the element ${pprint.apply(quotes.reflect.asTerm(expr))} into the Quill Abstract Syntax Tree") }
    /** For things that contain subclasses, don't strictly check the super type and fail the match
      * if the type fails since we can't do that when it could be a sub-type of the type being matched.
      * The only thing we can do in that case is go through the sub-parser and see if anything matches.
      */
    def unapply(expr: Expr[T])(using Quotes): Option[T] = unlift.lift(expr)
    // TODO Maybe want to go to stricter version of this going back to Some(apply(expr)). See comment below about quoted matching being covariant.
  }

  given unliftVisibility: NiceUnliftable[Visibility] with
    def unlift =
      case '{ Visibility.Hidden } => Visibility.Hidden
      case '{ Visibility.Visible } => Visibility.Visible

  given unliftRenameable: NiceUnliftable[Renameable] with
    def unlift =
      case '{ Renameable.ByStrategy } => Renameable.ByStrategy
      case '{ Renameable.Fixed } => Renameable.Fixed

  given unliftIdent: NiceUnliftable[AIdent] with
    def unlift =
      case '{ AIdent(${Const(name: String)}) } => AIdent(name)

  given unliftJoinType: NiceUnliftable[JoinType] with
    def unlift =
      case '{ InnerJoin } => InnerJoin
      case '{ LeftJoin } => LeftJoin
      case '{ RightJoin } => RightJoin
      case '{ FullJoin } => FullJoin

  given unliftProperty: NiceUnliftable[Property] with
    // Unlike in liftProperty, we need both variants here since we are matching the scala AST expressions
    def unlift =
      case '{ Property(${ast}, ${name}) } =>
        Property(ast.unexpr, constString(name))
      case '{ Property.Opinionated(${ast}, ${name}, ${renameable}, ${visibility}) } =>
        Property.Opinionated(ast.unexpr, constString(name), unliftRenameable(renameable), unliftVisibility(visibility))

  given unliftAssignment: NiceUnliftable[Assignment] with {
    def unlift =
      case '{ Assignment($alias, $property, $value) } => Assignment(alias.unexpr, property.unexpr, value.unexpr)
  }

  given unliftPropertyAlias: NiceUnliftable[PropertyAlias] with {
    def unlift =
      case '{ PropertyAlias($paths, $b) } => PropertyAlias(paths.unexpr, b.unexpr)
  }

  given unliftOptionOperation: NiceUnliftable[OptionOperation] with {
    def unlift =
      case '{ OptionApply.apply($a) } => OptionApply(a.unexpr)
      case '{ OptionSome.apply($a) } => OptionSome(a.unexpr)
      case '{ OptionNone } => OptionNone
      case '{ OptionIsEmpty.apply($a) } => OptionIsEmpty(a.unexpr)
      case '{ OptionMap.apply($a, $b, $c) } => OptionMap(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableMap.apply($a, $b, $c) } => OptionTableMap(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionExists.apply($a, $b, $c) } => OptionExists(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableExists.apply($a, $b, $c) } => OptionTableExists(a.unexpr, b.unexpr, c.unexpr)
  }

  def constString(expr: Expr[String])(using Quotes): String = expr match
    case Const(str: String) => str
    case _ => throw new IllegalArgumentException(s"The expression: ${expr.show} is not a constant String")


  given unliftAst: NiceUnliftable[Ast] with {
    // TODO have a typeclass like Splicer to translate constant to strings
    def unlift =
      case '{ Constant(${Const(b)}: Double) } => Constant(b)
      case '{ Constant(${Const(b)}: Boolean) } => Constant(b)
      case '{ Constant(${Const(b)}: String) } => Constant(b)
      case '{ Constant(${Const(b)}: Int) } => Constant(b)
      case '{ Entity.apply(${Const(b: String)}, ${elems})  } =>
        Entity(b, elems.unexpr)
      case '{ Function($params, $body) } => Function(params.unexpr, body.unexpr)
      case '{ FunctionApply($function, $values) } => FunctionApply(function.unexpr, values.unexpr)
      case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(query.unexpr, alias.unexpr, body.unexpr)
      case '{ FlatMap(${query}, ${alias}, ${body}: Ast) } => FlatMap(query.unexpr, alias.unexpr, body.unexpr)
      case '{ Filter(${query}, ${alias}, ${body}: Ast) } => Filter(query.unexpr, alias.unexpr, body.unexpr)
      case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(a.unexpr, unliftOperator(operator).asInstanceOf[BinaryOperator], b.unexpr)
      case '{ Property(${ast}, ${name}) } =>
        Property(ast.unexpr, constString(name))
      case '{ScalarTag(${uid})} => 
        ScalarTag(constString(uid))
      case '{ QuotationTag($uid) } =>
        QuotationTag(constString(uid))
      case '{ Union($a, $b) } => Union(a.unexpr, b.unexpr)
      case '{ Insert($query, $assignments) } => Insert(query.unexpr, assignments.unexpr)
      case '{ Infix($parts, $params, $pure) } => Infix(parts.unexpr, params.unexpr, pure.unexpr)
      case '{ Tuple.apply($values) } => 
        // println("((((((((((((((( ATTEMPTING TO EXTRACT TUPLE )))))))))))))))))))")
        // import quotes.reflect._
        // values match {
        //   case '{ List.apply[Ast]($args) } =>
        //     println(s"((((((((((((((( APPLY MATCH: ${Printer.TreeStructure.show(args.asTerm)} )))))))))))))))))))")
            
        //   case _ => 
        //     println("((((((((((((((( NO APPLY MATCH )))))))))))))))))))")
        // }
        Tuple(values.unexpr)
      case '{ Join($typ, $a, $b, $aliasA, $aliasB, $on) } => Join(typ.unexpr, a.unexpr, b.unexpr, aliasA.unexpr, aliasB.unexpr, on.unexpr)
      case '{ FlatJoin($typ, $a, $aliasA, $on) } => FlatJoin(typ.unexpr, a.unexpr, aliasA.unexpr, on.unexpr)
      case '{ CaseClass($values) } => CaseClass(values.unexpr)
      case '{ NullValue } => NullValue
      case '{ $p: Property } => 
        println("((((((((((((((( ATTEMPTING TO EXTRACT PROPERTY )))))))))))))))))))")
        unliftProperty(p)
      case '{ $id: AIdent } => unliftIdent(id)
      // TODO Is the matching covariant? In that case can do "case '{ $oo: OptionOperation } and then strictly throw an error"
      case unliftOptionOperation(ast) => ast
  }

  given unliftOperator: NiceUnliftable[Operator] with {
    def unlift =
      case '{ NumericOperator.+ } =>  NumericOperator.+
      case '{ NumericOperator.- } =>  NumericOperator.-
      case '{ NumericOperator.* } =>  NumericOperator.*
      case '{ NumericOperator./ } =>  NumericOperator./
      case '{ NumericOperator.% } =>  NumericOperator.%
      case '{ NumericOperator.> } =>  NumericOperator.>
      case '{ NumericOperator.< } =>  NumericOperator.<
      case '{ StringOperator.+ } =>  StringOperator.+
      case '{ EqualityOperator.== } =>  EqualityOperator.==
      case '{ BooleanOperator.|| } =>  BooleanOperator.||
      case '{ BooleanOperator.&& } =>  BooleanOperator.&&
  }

}