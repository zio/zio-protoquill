package io.getquill.parser

import scala.quoted.{ Type => TType, _ }
import scala.reflect.ClassTag
import scala.reflect.classTag;
import io.getquill.quat.Quat
import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}
import io.getquill.metaprog.Is
import io.getquill.metaprog.Extractors
import io.getquill.util.Format
import io.getquill.util.StringUtil.section
import io.getquill.metaprog.Extractors.MatchingOptimizers._

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
      import quotes.reflect._
      expr match
        case '{ SerialHelper.fromSerializedJVM[tt](${Expr(serial: String)}) } if (TypeRepr.of[tt] <:< TypeRepr.of[Ast]) =>
          SerialHelper.fromSerializedJVM[Ast](serial).asInstanceOf[T]
        // On JVM, a Quat must be serialized and then lifted from the serialized state i.e. as a FromSerialized using JVM (due to 64KB method limit)
        case '{ Quat.Product.fromSerializedJVM(${Expr(str: String)}) } =>
          Quat.Product.fromSerializedJVM(str).asInstanceOf[T]
        case '{ Quat.fromSerializedJVM(${Expr(str: String)}) } =>
          Quat.fromSerializedJVM(str).asInstanceOf[T]
        case _ =>
          unlift.lift(expr).getOrElse {
            report.throwError(
              s"Could not Unlift AST type ${classTag[T].runtimeClass.getSimpleName} from the element:\n" +
              s"${section(Format.Expr.Detail(expr))}\n" +
              s"of the Quill Abstract Syntax Tree",
              expr
            )
          }

    /** For things that contain subclasses, don't strictly check the super type and fail the match
      * if the type fails since we can't do that when it could be a sub-type of the type being matched.
      * The only thing we can do in that case is go through the sub-parser and see if anything matches.
      */
    def unapply(expr: Expr[T])(using Quotes): Option[T] = Some(apply(expr))
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
      case '{ AIdent.Opinionated(${Expr(name: String)}, $quat, $visibility) } =>
        val unliftedQuat = quat.unexpr
        AIdent.Opinionated(name, unliftedQuat, visibility.unexpr)

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

  given unliftAssignment: NiceUnliftable[Assignment] with
    def unlift =
      case '{ Assignment($alias, $property, $value) } => Assignment(alias.unexpr, property.unexpr, value.unexpr)

  given unliftAssignmentDual: NiceUnliftable[AssignmentDual] with
    def unlift =
      case '{ AssignmentDual($alias1, $alias2, $property, $value) } => AssignmentDual(alias1.unexpr, alias2.unexpr, property.unexpr, value.unexpr)

  given unliftPropertyAlias: NiceUnliftable[PropertyAlias] with {
    def unlift =
      case '{ PropertyAlias($paths, $b) } => PropertyAlias(paths.unexpr, b.unexpr)
  }

  given unliftTraversableOperation: NiceUnliftable[IterableOperation] with {
    def unlift =
      case '{ MapContains($a, $b) } => MapContains(a.unexpr, b.unexpr)
      case '{ SetContains($a, $b) } => SetContains(a.unexpr, b.unexpr)
      case '{ ListContains($a, $b) } => ListContains(a.unexpr, b.unexpr)
  }

  given unliftOptionOperation: NiceUnliftable[OptionOperation] with {
    def unlift =
      case Is[OptionApply]( '{ OptionApply.apply($a) } ) => OptionApply(a.unexpr)
      case Is[OptionSome]( '{ OptionSome.apply($a) } ) => OptionSome(a.unexpr)
      case Is[OptionNone]( '{ OptionNone($quat) } ) =>
        // Performance optimization, same as Ident. Do quat.unexper once instead of each time on
        // OptionNone.quat which would otherwise happen if quat.unexper would be passed directly.
        val unliftedQuat = quat.unexpr
        OptionNone(unliftedQuat)
      case Is[OptionIsEmpty]( '{ OptionIsEmpty.apply($a) } ) => OptionIsEmpty(a.unexpr)
      case Is[OptionNonEmpty]( '{ OptionNonEmpty.apply($a) } ) => OptionNonEmpty(a.unexpr)
      case Is[OptionIsDefined]( '{ OptionIsDefined.apply($a) } ) => OptionIsDefined(a.unexpr)
      case Is[OptionGetOrElse]( '{ OptionGetOrElse.apply($a, $b) } ) => OptionGetOrElse(a.unexpr, b.unexpr)
      case Is[OptionContains]( '{ OptionContains.apply($a, $b) } ) => OptionContains(a.unexpr, b.unexpr)
      case Is[OptionMap]( '{ OptionMap.apply($a, $b, $c) } ) => OptionMap(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionFlatMap]( '{ OptionFlatMap.apply($a, $b, $c) } ) => OptionFlatMap(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionFlatten]( '{ OptionFlatten.apply($a) } ) => OptionFlatten(a.unexpr)
      case Is[OptionTableMap]( '{ OptionTableMap.apply($a, $b, $c) } ) => OptionTableMap(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionTableFlatMap]( '{ OptionTableFlatMap.apply($a, $b, $c) } ) => OptionTableFlatMap(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionExists]( '{ OptionExists.apply($a, $b, $c) } ) => OptionExists(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionForall]( '{ OptionForall.apply($a, $b, $c) } ) => OptionForall(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionTableExists]( '{ OptionTableExists.apply($a, $b, $c) } ) => OptionTableExists(a.unexpr, b.unexpr, c.unexpr)
      case Is[OptionTableForall]( '{ OptionTableForall.apply($a, $b, $c) } ) => OptionTableForall(a.unexpr, b.unexpr, c.unexpr)
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

  given unliftConstant: NiceUnliftable[Constant] with
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
      case '{ Constant(${Expr(b: Char)}: Char, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Int)}: Int, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Long)}: Long, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Short)}: Short, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Float)}: Float, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Double)}: Double, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant(${Expr(b: Byte)}: Byte, $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant(b, unliftedQuat)
      case '{ Constant((), $quat) } =>
        val unliftedQuat = quat.unexpr // Performance optimization, same as Ident and Entity
        Constant((), unliftedQuat)

  given unliftAction: NiceUnliftable[Action] with
    def unlift =
      case Is[Update]( '{ Update($query, $assignments) } ) => Update(query.unexpr, assignments.unexpr)
      case Is[Insert]( '{ Insert($query, $assignments) } ) => Insert(query.unexpr, assignments.unexpr)
      case Is[Delete]( '{ Delete($query) } ) => Delete(query.unexpr)
      case Is[Returning]( '{ Returning(${action}, ${alias}, ${body}: Ast) } ) => Returning(action.unexpr, alias.unexpr, body.unexpr)
      case Is[ReturningGenerated]( '{ ReturningGenerated(${action}, ${alias}, ${body}: Ast) } ) => ReturningGenerated(action.unexpr, alias.unexpr, body.unexpr)
      case Is[Foreach]( '{ Foreach(${query}, ${alias}, ${body}: Ast) } ) => Foreach(query.unexpr, alias.unexpr, body.unexpr)
      case Is[OnConflict]( '{ OnConflict($a, $b, $c) } ) => OnConflict(a.unexpr, b.unexpr, c.unexpr)


  given unliftConflictTarget: NiceUnliftable[OnConflict.Target] with
    def unlift =
      case '{ OnConflict.NoTarget } => OnConflict.NoTarget
      case '{ OnConflict.Properties($a) } => OnConflict.Properties(a.unexpr)

  given unliftConflictAction: NiceUnliftable[OnConflict.Action] with
    def unlift =
      case '{ OnConflict.Ignore } => OnConflict.Ignore
      case '{ OnConflict.Update($a) } => OnConflict.Update(a.unexpr)

  given unliftQuery: NiceUnliftable[AQuery] with
    def unlift =
      case Is[Entity](ent) => unliftEntity(ent)
      case Is[Map]( '{ Map(${query}, ${alias}, ${body}: Ast) } ) => Map(query.unexpr, alias.unexpr, body.unexpr)
      case Is[FlatMap]( '{ FlatMap(${query}, ${alias}, ${body}: Ast) } ) => FlatMap(query.unexpr, alias.unexpr, body.unexpr)
      case Is[Filter]( '{ Filter(${query}, ${alias}, ${body}: Ast) } ) => Filter(query.unexpr, alias.unexpr, body.unexpr)
      case Is[GroupBy]( '{ GroupBy(${query}, ${alias}, ${body}: Ast) } ) => GroupBy(query.unexpr, alias.unexpr, body.unexpr)
      case Is[SortBy]( '{ SortBy(${query}, ${alias}, ${criterias}, ${ordering}) } ) => SortBy(query.unexpr, alias.unexpr, criterias.unexpr, ordering.unexpr)
      case Is[Distinct]( '{ Distinct(${a}) } )  => Distinct(a.unexpr)
      case Is[Nested]( '{ Nested(${a}) } ) => Nested(a.unexpr)
      case Is[Union]( '{ Union($a, $b) } ) => Union(a.unexpr, b.unexpr)
      case Is[UnionAll]( '{ UnionAll($a, $b) } ) => UnionAll(a.unexpr, b.unexpr)
      case Is[Join]( '{ Join($typ, $a, $b, $aliasA, $aliasB, $on) } ) => Join(typ.unexpr, a.unexpr, b.unexpr, aliasA.unexpr, aliasB.unexpr, on.unexpr)
      case Is[FlatJoin]( '{ FlatJoin($typ, $a, $aliasA, $on) } ) => FlatJoin(typ.unexpr, a.unexpr, aliasA.unexpr, on.unexpr)
      case Is[Take]( '{ Take($query, $num)} ) => Take(query.unexpr, num.unexpr)
      case Is[Drop]( '{ Drop($query, $num)} ) => Drop(query.unexpr, num.unexpr)
      case Is[ConcatMap]( '{ ConcatMap(${query}, ${alias}, ${body}: Ast) } ) => ConcatMap(query.unexpr, alias.unexpr, body.unexpr)
      // Note: Aggregation is actually a Query-Type. Not sure why in Scala2-Quill it's not in the query-unlifter
      case Is[Aggregation]( '{ Aggregation(${operator}, ${query}) } ) => Aggregation(operator.unexpr, query.unexpr)

  given unliftEntity: NiceUnliftable[Entity] with
    def unlift =
      case Is[Entity]( '{ Entity.apply(${Expr(b: String)}, $elems, $quat) } ) =>
        // Performance optimization, same as for Ident. Entity.quat is by-name so make sure to do unexper once here.
        val unliftedQuat = quat.unexpr
        Entity(b, elems.unexpr, unliftedQuat)
      case Is[Entity]( '{ Entity.Opinionated.apply(${Expr(b: String)}, $elems, $quat, $renameable) } ) =>
        // Performance optimization, same as for Ident. Entity.quat is by-name so make sure to do unexper once here.
        val unliftedQuat = quat.unexpr
        Entity.Opinionated(b, elems.unexpr, unliftedQuat, renameable.unexpr)

  given unliftAst: NiceUnliftable[Ast] with {
    def unlift =
      case Is[AQuery](q) => unliftQuery(q)
      case Is[Constant](c) => unliftConstant(c)
      case Is[Action](a) => unliftAction(a)
      case Is[Entity](p) => unliftEntity(p)
      case Is[Property](p) => unliftProperty(p)
      case Is[AIdent](i) => unliftIdent(i)
      case Is[Ordering](o) => unliftOrdering(o)
      case Is[If]( '{ If($cond, $thenStmt, $elseStmt) } ) => If(cond.unexpr, thenStmt.unexpr, elseStmt.unexpr)
      case Is[Function]( '{ Function($params, $body) } ) => Function(params.unexpr, body.unexpr)
      case Is[FunctionApply]( '{ FunctionApply($function, $values) } ) => FunctionApply(function.unexpr, values.unexpr)
      case Is[UnaryOperation]( '{ UnaryOperation(${operator}, ${a}: Ast) } ) => UnaryOperation(unliftOperator(operator).asInstanceOf[UnaryOperator], a.unexpr)
      case Is[BinaryOperation]( '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } ) => BinaryOperation(a.unexpr, unliftOperator(operator).asInstanceOf[BinaryOperator], b.unexpr)
      case Is[Property]( '{ Property(${ast}, ${name}) } ) => Property(ast.unexpr, constString(name))
      case Is[ScalarTag]( '{ScalarTag(${uid})} ) => ScalarTag(constString(uid))
      case Is[QuotationTag]( '{ QuotationTag($uid) } ) => QuotationTag(constString(uid))
      case Is[Infix]( '{ Infix($parts, $params, $pure, $quat) } ) => Infix(parts.unexpr, params.unexpr, pure.unexpr, quat.unexpr)
      case Is[Tuple]( '{ Tuple.apply($values) } ) => Tuple(values.unexpr)
      case Is[CaseClass]( '{ CaseClass($values) } ) => CaseClass(values.unexpr)
      case Is[IterableOperation]( unliftTraversableOperation(o) ) => o
      // TODO Is the matching covariant? In that case can do "case '{ $oo: OptionOperation } and then strictly throw an error"
      case Is[OptionOperation]( ast ) => unliftOptionOperation(ast)
      case Is[Assignment]( ast ) => unliftAssignment(ast)
      case Is[OnConflict.Excluded]( '{ OnConflict.Excluded($a) } ) => OnConflict.Excluded(a.unexpr)
      case Is[OnConflict.Existing]( '{ OnConflict.Existing($a) } ) => OnConflict.Existing(a.unexpr)
      case '{ NullValue } => NullValue
  }

  given unliftOperator: NiceUnliftable[Operator] with {
    def unlift =
      case '{ SetOperator.contains } => SetOperator.contains
      case '{ SetOperator.nonEmpty } => SetOperator.nonEmpty
      case '{ SetOperator.isEmpty } => SetOperator.isEmpty
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
      case '{ StringOperator.toInt } =>  StringOperator.toInt
      case '{ StringOperator.startsWith } =>  StringOperator.startsWith
      case '{ StringOperator.split } =>  StringOperator.split
      case '{ EqualityOperator.== } =>  EqualityOperator.==
      case '{ EqualityOperator.!= } =>  EqualityOperator.!=
      case '{ BooleanOperator.|| } =>  BooleanOperator.||
      case '{ BooleanOperator.&& } =>  BooleanOperator.&&
      case '{ BooleanOperator.! } =>  BooleanOperator.!
  }

  given quatProductTypeUnliftable: NiceUnliftable[Quat.Product.Type] with {
    def unlift =
      case '{ Quat.Product.Type.Concrete } => Quat.Product.Type.Concrete
      case '{ Quat.Product.Type.Abstract } => Quat.Product.Type.Abstract
  }

  extension [T](expr: Seq[Expr[T]])(using FromExpr[T], Quotes)
    def unexprSeq = expr.map(_.valueOrError)

  given quatProductUnliftable: NiceUnliftable[Quat.Product] with {
    def unlift =
      case '{ Quat.Product.WithRenamesCompact.apply($tpe)(${Varargs(fields)}: _*)(${Varargs(values)}: _*)(${Varargs(renamesFrom)}: _*)(${Varargs(renamesTo)}: _*) } => Quat.Product.WithRenamesCompact(tpe.unexpr)(fields.unexprSeq: _*)(values.unexprSeq: _*)(renamesFrom.unexprSeq: _*)(renamesTo.unexprSeq: _*)
      //case '{ Quat.Product.apply(${Varargs(fields)}: _*) } => Quat.Product(fields.unexprSeq: _*)
  }

  given quatUnliftable: NiceUnliftable[Quat] with {
    def unlift =
      case Is[Quat.Product](p) => quatProductUnliftable(p)
      case '{ Quat.Value } => Quat.Value
      case '{ Quat.Null } => Quat.Null
      case '{ Quat.Generic } => Quat.Generic
      case '{ Quat.Unknown } => Quat.Unknown
      case '{ Quat.BooleanValue } => Quat.BooleanValue
      case '{ Quat.BooleanExpression } => Quat.BooleanExpression
  }

}
