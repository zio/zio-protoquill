package io.getquill.parser

import scala.quoted.{ Type => TType, _ }
import scala.reflect.ClassTag
import scala.reflect.classTag;
import io.getquill.quat.Quat
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

  given unliftAggregation: NiceUnliftable[AggregationOperator] with
    def unlift =
      case '{ AggregationOperator.`min` }  =>  AggregationOperator.`min`
      case '{ AggregationOperator.`max` }  =>  AggregationOperator.`max`
      case '{ AggregationOperator.`avg` }  =>  AggregationOperator.`avg`
      case '{ AggregationOperator.`sum` }  =>  AggregationOperator.`sum`
      case '{ AggregationOperator.`size` } =>  AggregationOperator.`size`

  given unliftRenameable: NiceUnliftable[Renameable] with
    def unlift =
      case '{ Renameable.ByStrategy } => Renameable.ByStrategy
      case '{ Renameable.Fixed } => Renameable.Fixed

  given unliftIdent: NiceUnliftable[AIdent] with
    def unlift =
      case '{ AIdent(${Expr(name: String)}, $quat) } =>
        // Performance optimization! Since Ident.quat is a by-name parameter, unless we force it to unexpr here,
        // it will be done over and over again each time quat.unexpr is called which is extremely wasteful.
        val unliftedQuat = quat.unexpr
        AIdent(name, unliftedQuat)

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
      case '{ OptionNone($quat) } => 
        // Performance optimization, same as Ident. Do quat.unexper once instead of each time on 
        // OptionNone.quat which would otherwise happen if quat.unexper would be passed directly.
        val unliftedQuat = quat.unexpr
        OptionNone(unliftedQuat)
      case '{ OptionIsEmpty.apply($a) } => OptionIsEmpty(a.unexpr)
      case '{ OptionNonEmpty.apply($a) } => OptionNonEmpty(a.unexpr)
      case '{ OptionIsDefined.apply($a) } => OptionIsDefined(a.unexpr)
      case '{ OptionGetOrElse.apply($a, $b) } => OptionGetOrElse(a.unexpr, b.unexpr)
      case '{ OptionContains.apply($a, $b) } => OptionContains(a.unexpr, b.unexpr)
      case '{ OptionMap.apply($a, $b, $c) } => OptionMap(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableMap.apply($a, $b, $c) } => OptionTableMap(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableFlatMap.apply($a, $b, $c) } => OptionTableFlatMap(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionExists.apply($a, $b, $c) } => OptionExists(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionForall.apply($a, $b, $c) } => OptionForall(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableExists.apply($a, $b, $c) } => OptionTableExists(a.unexpr, b.unexpr, c.unexpr)
      case '{ OptionTableForall.apply($a, $b, $c) } => OptionTableForall(a.unexpr, b.unexpr, c.unexpr)
  }

  def constString(expr: Expr[String])(using Quotes): String = expr match
    case Expr(str: String) => str
    case _ => throw new IllegalArgumentException(s"The expression: ${expr.show} is not a constant String")

  given unliftOrdering: NiceUnliftable[Ordering] with
    def unlift =
      case '{ io.getquill.ast.TupleOrdering.apply($elems) } => TupleOrdering(elems.unexpr)
      case '{ io.getquill.ast.Asc } => Asc
      case '{ io.getquill.ast.Desc } => Desc
      case '{ io.getquill.ast.AscNullsFirst } => AscNullsFirst
      case '{ io.getquill.ast.DescNullsFirst } => DescNullsFirst
      case '{ io.getquill.ast.AscNullsLast } => AscNullsLast
      case '{ io.getquill.ast.DescNullsLast } => DescNullsLast


  given unliftAst: NiceUnliftable[Ast] with {
    // TODO have a typeclass like Splicer to translate constant to strings
    def unlift =
      case '{ Constant(${Expr(b: Double)}: Double, $quat) } => 
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Boolean)}: Boolean, $quat) } => 
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: String)}: String, $quat) } => 
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Int)}: Int, $quat) } => 
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Entity.apply(${Expr(b: String)}, $elems, $quat)  } =>
        // Performance optimization, same as for Ident. Entity.quat is by-name so make sure to do unexper once here.
        val unliftedQuat = quat.unexpr
        Entity(b, elems.unexpr, unliftedQuat)
      case '{ Function($params, $body) } => Function(params.unexpr, body.unexpr)
      case '{ FunctionApply($function, $values) } => FunctionApply(function.unexpr, values.unexpr)
      case '{ Aggregation(${operator}, ${query}) } => Aggregation(operator.unexpr, query.unexpr)
      case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(query.unexpr, alias.unexpr, body.unexpr)
      case '{ FlatMap(${query}, ${alias}, ${body}: Ast) } => FlatMap(query.unexpr, alias.unexpr, body.unexpr)
      case '{ Filter(${query}, ${alias}, ${body}: Ast) } => Filter(query.unexpr, alias.unexpr, body.unexpr)
      case '{ SortBy(${query}, ${alias}, ${criterias}, ${ordering}) } => SortBy(query.unexpr, alias.unexpr, criterias.unexpr, ordering.unexpr)
      case '{ Foreach(${query}, ${alias}, ${body}: Ast) } => Foreach(query.unexpr, alias.unexpr, body.unexpr)
      case '{ UnaryOperation(${operator}, ${a}: Ast) } => UnaryOperation(unliftOperator(operator).asInstanceOf[UnaryOperator], a.unexpr)
      case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(a.unexpr, unliftOperator(operator).asInstanceOf[BinaryOperator], b.unexpr)
      case '{ Property(${ast}, ${name}) } =>
        Property(ast.unexpr, constString(name))
      case '{ScalarTag(${uid})} => 
        ScalarTag(constString(uid))
      case '{ QuotationTag($uid) } =>
        QuotationTag(constString(uid))
      case '{ Union($a, $b) } => Union(a.unexpr, b.unexpr)
      case '{ Insert($query, $assignments) } => Insert(query.unexpr, assignments.unexpr)
      case '{ Update($query, $assignments) } => Update(query.unexpr, assignments.unexpr)
      case '{ Delete($query) } => Delete(query.unexpr)
      case '{ Returning(${action}, ${alias}, ${body}: Ast) } => Returning(action.unexpr, alias.unexpr, body.unexpr)
      case '{ ReturningGenerated(${action}, ${alias}, ${body}: Ast) } => ReturningGenerated(action.unexpr, alias.unexpr, body.unexpr)
      case '{ Infix($parts, $params, $pure, $quat) } => Infix(parts.unexpr, params.unexpr, pure.unexpr, quat.unexpr)
      case '{ Tuple.apply($values) } => Tuple(values.unexpr)
      case '{ Join($typ, $a, $b, $aliasA, $aliasB, $on) } => Join(typ.unexpr, a.unexpr, b.unexpr, aliasA.unexpr, aliasB.unexpr, on.unexpr)
      case '{ FlatJoin($typ, $a, $aliasA, $on) } => FlatJoin(typ.unexpr, a.unexpr, aliasA.unexpr, on.unexpr)
      case '{ Take($query, $num)} => Take(query.unexpr, num.unexpr)
      case '{ Drop($query, $num)} => Drop(query.unexpr, num.unexpr)
      case '{ ConcatMap(${query}, ${alias}, ${body}: Ast) } => ConcatMap(query.unexpr, alias.unexpr, body.unexpr)
      case '{ CaseClass($values) } => CaseClass(values.unexpr)
      case '{ NullValue } => NullValue
      case '{ $p: Property } => unliftProperty(p)
      case '{ $id: AIdent } => unliftIdent(id)
      case '{ $o: Ordering } => unliftOrdering(o)
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
      case '{ NumericOperator.>= } =>  NumericOperator.>=
      case '{ NumericOperator.<= } =>  NumericOperator.<=
      case '{ StringOperator.+ } =>  StringOperator.+
      case '{ StringOperator.toUpperCase } =>  StringOperator.toUpperCase
      case '{ StringOperator.toLowerCase } =>  StringOperator.toLowerCase
      case '{ StringOperator.toLong } =>  StringOperator.toLong
      case '{ StringOperator.startsWith } =>  StringOperator.startsWith
      case '{ StringOperator.split } =>  StringOperator.split
      case '{ EqualityOperator.== } =>  EqualityOperator.==
      case '{ EqualityOperator.!= } =>  EqualityOperator.!=
      case '{ BooleanOperator.|| } =>  BooleanOperator.||
      case '{ BooleanOperator.&& } =>  BooleanOperator.&&
  }

  given quatProductTypeUnliftable: NiceUnliftable[Quat.Product.Type] with {
    def unlift =
      case '{ Quat.Product.Type.Concrete } => Quat.Product.Type.Concrete
      case '{ Quat.Product.Type.Abstract } => Quat.Product.Type.Abstract
  }

  extension [T](expr: Seq[Expr[T]])(using FromExpr[T], Quotes)
    def unexprSeq = expr.map(_.valueOrError)

  given quatProductUnliftable: NiceUnliftable[Quat.Product] with {
    // On JVM, a Quat must be serialized and then lifted from the serialized state i.e. as a FromSerialized using JVM (due to 64KB method limit)
    def unlift =
      case '{ Quat.Product.fromSerializedJVM(${Expr(str: String)}) } => Quat.Product.fromSerializedJVM(str)
      case '{ Quat.Product.WithRenamesCompact.apply($tpe)(${Varargs(fields)}: _*)(${Varargs(values)}: _*)(${Varargs(renamesFrom)}: _*)(${Varargs(renamesTo)}: _*) } => Quat.Product.WithRenamesCompact(tpe.unexpr)(fields.unexprSeq: _*)(values.unexprSeq: _*)(renamesFrom.unexprSeq: _*)(renamesTo.unexprSeq: _*)
  }

  given quatUnliftable: NiceUnliftable[Quat] with {
    def unlift =
      // On JVM, a Quat must be serialized and then lifted from the serialized state i.e. as a FromSerialized using JVM (due to 64KB method limit)
      case '{ Quat.fromSerializedJVM(${Expr(str: String)}) } => Quat.fromSerializedJVM(str)
      case '{ Quat.Product.WithRenamesCompact.apply($tpe)(${Varargs(fields)}: _*)(${Varargs(values)}: _*)(${Varargs(renamesFrom)}: _*)(${Varargs(renamesTo)}: _*) } => Quat.Product.WithRenamesCompact(tpe.unexpr)(fields.unexprSeq: _*)(values.unexprSeq: _*)(renamesFrom.unexprSeq: _*)(renamesTo.unexprSeq: _*)
      
      // TODO Ask Nicolas How do you uniquely identify this?
      //case '{ Quat.Product.apply(${Varargs(fields)}: _*) } => Quat.Product(fields.unexprSeq: _*)
      case '{ Quat.Value } => Quat.Value
      case '{ Quat.Null } => Quat.Null
      case '{ Quat.Generic } => Quat.Generic
      case '{ Quat.Unknown } => Quat.Unknown
      case '{ Quat.BooleanValue } => Quat.BooleanValue
      case '{ Quat.BooleanExpression } => Quat.BooleanExpression
  }

}