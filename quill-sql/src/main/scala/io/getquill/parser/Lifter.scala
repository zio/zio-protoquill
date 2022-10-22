package io.getquill.parser

import scala.quoted.{Type => TType, _}
import scala.quoted
import io.getquill.ast.{Ident => AIdent, Query => AQuery, _}
import io.getquill.metaprog.Extractors._
import scala.reflect.ClassTag;
import scala.reflect.classTag;
import io.getquill.quat.Quat
import io.getquill.util.Messages
import io.getquill.ReturnAction
import scala.util.Try
import io.getquill.util.Format
import io.getquill.util.StringUtil.section
import io.getquill.parser.DoSerialize
import io.getquill.ast
import io.getquill.util.CommonExtensions.Throwable._

/**
 * Convert constructs of Quill Ast into Expr[Ast]. This allows them to be passed
 * back an fourth between inline Quotation blocks during compile-time which should eventually
 * be bassed into a run-call-site where they will be evaluated into SQL.
 *
 * Note that liftable List is already taken care of by the Dotty implicits
 */
case class Lifter(serializeQuat: SerializeQuat, serializeAst: SerializeAst) extends Lifters.Proxy {
  val default = this

  extension [T](t: T)(using ToExpr[T], Quotes)
    def expr: Expr[T] = Expr(t)

  trait LiftAstSerialize[T <: Ast: ClassTag] extends ToExpr[T] with Lifters.WithSerializing.Ast[T] with Lifters.Plain.Ast[T]:
    def typeTag: Quotes ?=> TType[T]
    def lift: Quotes ?=> PartialFunction[T, Expr[T]]

    // The primary entry-point for external usage
    override def orFail(element: T)(using Quotes): Expr[T] =
      given TType[T] = typeTag
      if (serializeAst == SerializeAst.None)
        liftPlainOrFail(element)
      else if (hasSerializeDisabledTypeclass)
        Lifter(SerializeQuat.None, SerializeAst.None).liftAst(element).asInstanceOf[Expr[T]]
      else if (serializeAst == SerializeAst.All)
        tryLiftSerialized(element).getOrElse { liftPlainOrFail(element) }
      else
        liftPlainOrFail(element)
  end LiftAstSerialize

  trait LiftQuatSerialize[T <: Quat: ClassTag] extends ToExpr[T] with Lifters.WithSerializing.Quat[T] with Lifters.Plain.Quat[T]:
    def typeTag: Quotes ?=> TType[T]
    def lift: Quotes ?=> PartialFunction[T, Expr[T]]

    // The primary entry-point for external usage
    override def orFail(element: T)(using Quotes): Expr[T] =
      given TType[T] = typeTag
      if (serializeQuat == SerializeQuat.None)
        liftPlainOrFail(element)
      else if (hasSerializeDisabledTypeclass)
        Lifter(SerializeQuat.None, SerializeAst.None).liftQuat(element).asInstanceOf[Expr[T]]
      else if (serializeQuat == SerializeQuat.All)
        tryLiftSerialized(element).getOrElse { liftPlainOrFail(element) }
      else
        liftPlainOrFail(element)
  end LiftQuatSerialize

  // Technically not part of the AST this needs to be lifted in the QueryExecution and returned to the executeActionReturning context clause
  given liftReturnAction: Lifters.Plain[ReturnAction] with
    def lift =
      case ReturnAction.ReturnNothing         => '{ ReturnAction.ReturnNothing }
      case ReturnAction.ReturnColumns(colums) => '{ ReturnAction.ReturnColumns(${ colums.expr }) }
      case ReturnAction.ReturnRecord          => '{ ReturnAction.ReturnRecord }

  given liftRenameable: Lifters.Plain[Renameable] with
    def lift =
      case Renameable.ByStrategy => '{ Renameable.ByStrategy }
      case Renameable.Fixed      => '{ Renameable.Fixed }

  given liftVisbility: Lifters.Plain[Visibility] with {
    def lift =
      case Visibility.Visible => '{ Visibility.Visible }
      case Visibility.Hidden  => '{ Visibility.Hidden }
  }

  given liftAggregation: Lifters.Plain[AggregationOperator] with {
    def lift =
      case AggregationOperator.`min`  => '{ AggregationOperator.`min` }
      case AggregationOperator.`max`  => '{ AggregationOperator.`max` }
      case AggregationOperator.`avg`  => '{ AggregationOperator.`avg` }
      case AggregationOperator.`sum`  => '{ AggregationOperator.`sum` }
      case AggregationOperator.`size` => '{ AggregationOperator.`size` }
  }

  given liftProperty: LiftAstSerialize[Property] with {
    def typeTag = TType.of[Property]
    def lift = {
      // Don't need the other case since Property.Opinionated will match the object
      // Note: don't declare variable called 'ast' since it is used internally
      case Property.Opinionated(core: Ast, name: String, renameable: Renameable, visibility: Visibility) =>
        '{ Property.Opinionated(${ core.expr }, ${ name.expr }, ${ renameable.expr }, ${ visibility.expr }) }
    }
  }

  given liftIdent: LiftAstSerialize[AIdent] with {
    def typeTag = TType.of[AIdent]
    def lift =
      case AIdent.Opinionated(name: String, quat, visibility) =>
        '{ AIdent.Opinionated(${ name.expr }, ${ quat.expr }, ${ visibility.expr }) }
  }

  given liftPropertyAlias: Lifters.Plain[PropertyAlias] with {
    def lift =
      case PropertyAlias(a, b) => '{ PropertyAlias(${ a.expr }, ${ b.expr }) }
  }

  given liftAssignment: LiftAstSerialize[Assignment] with
    def typeTag = TType.of[Assignment]
    def lift =
      case Assignment(ident, property, value) => '{ Assignment(${ ident.expr }, ${ property.expr }, ${ value.expr }) }

  given liftAssignmentDual: LiftAstSerialize[AssignmentDual] with
    def typeTag = TType.of[AssignmentDual]
    def lift =
      case AssignmentDual(ident1, ident2, property, value) => '{ AssignmentDual(${ ident1.expr }, ${ ident2.expr }, ${ property.expr }, ${ value.expr }) }

  given liftJoinType: Lifters.Plain[JoinType] with {
    def lift =
      case InnerJoin => '{ InnerJoin }
      case LeftJoin  => '{ LeftJoin }
      case RightJoin => '{ RightJoin }
      case FullJoin  => '{ FullJoin }
  }

  given liftQuatProduct: LiftQuatSerialize[Quat.Product] with {
    def typeTag = TType.of[Quat.Product]
    def lift =
      case Quat.Product.WithRenamesCompact(name, tpe, fields, values, renamesFrom, renamesTo) =>
        '{
          io.getquill.quat.Quat.Product.WithRenamesCompact.apply(${ name.expr }, ${ tpe.expr })(${ fields.toList.spliceVarargs }: _*)(${ values.toList.spliceVarargs }: _*)(${ renamesFrom.toList.spliceVarargs }: _*)(${
            renamesTo.toList.spliceVarargs
          }: _*)
        }
  }

  extension [T: TType](list: List[T])(using ToExpr[T], Quotes)
    def spliceVarargs = Varargs(list.map(Expr(_)).toSeq)

  given liftQuat: LiftQuatSerialize[Quat] with {
    def typeTag = TType.of[Quat]
    def lift =
      case Quat.Product.WithRenamesCompact(name, tpe, fields, values, renamesFrom, renamesTo) => '{
          io.getquill.quat.Quat.Product.WithRenamesCompact.apply(${ name.expr }, ${ tpe.expr })(${ fields.toList.spliceVarargs }: _*)(${ values.toList.spliceVarargs }: _*)(${ renamesFrom.toList.spliceVarargs }: _*)(${
            renamesTo.toList.spliceVarargs
          }: _*)
        }
      case Quat.Value             => '{ io.getquill.quat.Quat.Value }
      case Quat.Null              => '{ io.getquill.quat.Quat.Null }
      case Quat.Generic           => '{ io.getquill.quat.Quat.Generic }
      case Quat.Unknown           => '{ io.getquill.quat.Quat.Unknown }
      case Quat.BooleanValue      => '{ io.getquill.quat.Quat.BooleanValue }
      case Quat.BooleanExpression => '{ io.getquill.quat.Quat.BooleanExpression }
  }

  given liftQuatProductType: Lifters.Plain[Quat.Product.Type] with {
    def lift =
      case Quat.Product.Type.Concrete => '{ io.getquill.quat.Quat.Product.Type.Concrete }
      case Quat.Product.Type.Abstract => '{ io.getquill.quat.Quat.Product.Type.Abstract }
  }

  given liftTraversableOperation: LiftAstSerialize[IterableOperation] with {
    def typeTag = TType.of[IterableOperation]
    def lift =
      case MapContains(a, b)  => '{ MapContains(${ a.expr }, ${ b.expr }) }
      case SetContains(a, b)  => '{ SetContains(${ a.expr }, ${ b.expr }) }
      case ListContains(a, b) => '{ ListContains(${ a.expr }, ${ b.expr }) }
  }

  given liftOptionOperation: LiftAstSerialize[OptionOperation] with {
    def typeTag = TType.of[OptionOperation]
    def lift =
      case OptionApply(a)              => '{ OptionApply(${ a.expr }) }
      case OptionSome(a)               => '{ OptionSome(${ a.expr }) }
      case OptionNone(quat)            => '{ OptionNone(${ quat.expr }) }
      case OptionIsEmpty(a)            => '{ OptionIsEmpty(${ a.expr }) }
      case OptionNonEmpty(a)           => '{ OptionNonEmpty(${ a.expr }) }
      case OptionIsDefined(a)          => '{ OptionIsDefined(${ a.expr }) }
      case OptionGetOrElse(a, b)       => '{ OptionGetOrElse(${ a.expr }, ${ b.expr }) }
      case OptionGetOrNull(a)          => '{ OptionGetOrNull(${ a.expr }) }
      case OptionOrNull(a)             => '{ OptionOrNull(${ a.expr }) }
      case FilterIfDefined(a, b, c)    => '{ FilterIfDefined(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionContains(a, b)        => '{ OptionContains(${ a.expr }, ${ b.expr }) }
      case OptionMap(a, b, c)          => '{ OptionMap(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionFlatMap(a, b, c)      => '{ OptionFlatMap(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionFlatten(a)            => '{ OptionFlatten(${ a.expr }) }
      case OptionTableMap(a, b, c)     => '{ OptionTableMap(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionTableFlatMap(a, b, c) => '{ OptionTableFlatMap(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionExists(a, b, c)       => '{ OptionExists(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionForall(a, b, c)       => '{ OptionForall(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionTableExists(a, b, c)  => '{ OptionTableExists(${ a.expr }, ${ b.expr }, ${ c.expr }) }
      case OptionTableForall(a, b, c)  => '{ OptionTableForall(${ a.expr }, ${ b.expr }, ${ c.expr }) }
  }

  given liftEntity: LiftAstSerialize[Entity] with
    def typeTag = TType.of[Entity]
    def lift =
      // case ast if (serializeAst == SerializeAst.All) => tryToSerialize[Entity](ast)
      case Entity.Opinionated(name: String, list, quat, renameable) => '{ Entity.Opinionated(${ name.expr }, ${ list.expr }, ${ quat.expr }, ${ renameable.expr }) }

  given liftCaseClass: LiftAstSerialize[CaseClass] with
    def typeTag = TType.of[CaseClass]
    def lift =
      case cc @ CaseClass(name, lifts) =>
        '{ CaseClass(${ name.expr }, ${ lifts.expr }) } // List lifter and tuple lifter come built in so can just do Expr(lifts) (or lifts.expr for short)

  given liftTuple: LiftAstSerialize[Tuple] with
    def typeTag = TType.of[Tuple]
    def lift =
      case Tuple(values) => '{ Tuple(${ values.expr }) }

  given orderingLiftable: LiftAstSerialize[Ordering] with
    def typeTag = TType.of[Ordering]
    def lift =
      case TupleOrdering(elems) => '{ io.getquill.ast.TupleOrdering(${ elems.expr }) }
      case Asc                  => '{ io.getquill.ast.Asc }
      case Desc                 => '{ io.getquill.ast.Desc }
      case AscNullsFirst        => '{ io.getquill.ast.AscNullsFirst }
      case DescNullsFirst       => '{ io.getquill.ast.DescNullsFirst }
      case AscNullsLast         => '{ io.getquill.ast.AscNullsLast }
      case DescNullsLast        => '{ io.getquill.ast.DescNullsLast }

  given liftAction: LiftAstSerialize[Action] with
    def typeTag = TType.of[Action]
    def lift =
      case Insert(query: Ast, assignments: List[Assignment])         => '{ Insert(${ query.expr }, ${ assignments.expr }) }
      case Update(query: Ast, assignments: List[Assignment])         => '{ Update(${ query.expr }, ${ assignments.expr }) }
      case Delete(query: Ast)                                        => '{ Delete(${ query.expr }) }
      case Returning(action: Ast, alias: AIdent, body: Ast)          => '{ Returning(${ action.expr }, ${ alias.expr }, ${ body.expr }) }
      case ReturningGenerated(action: Ast, alias: AIdent, body: Ast) => '{ ReturningGenerated(${ action.expr }, ${ alias.expr }, ${ body.expr }) }
      case Foreach(query: Ast, alias: AIdent, body: Ast)             => '{ Foreach(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case OnConflict(a, b, c)                                       => '{ OnConflict(${ a.expr }, ${ b.expr }, ${ c.expr }) }

  given liftConflictTarget: Lifters.Plain[OnConflict.Target] with
    def lift =
      case OnConflict.NoTarget      => '{ OnConflict.NoTarget }
      case OnConflict.Properties(a) => '{ OnConflict.Properties(${ a.expr }) }

  given liftConflictAction: Lifters.Plain[OnConflict.Action] with
    def lift =
      case OnConflict.Ignore    => '{ OnConflict.Ignore }
      case OnConflict.Update(a) => '{ OnConflict.Update(${ a.expr }) }

  given liftQuery: LiftAstSerialize[AQuery] with
    def typeTag = TType.of[AQuery]
    def lift =
      case e: Entity                                                        => liftEntity(e)
      case Filter(query: Ast, alias: AIdent, body: Ast)                     => '{ Filter(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case Map(query: Ast, alias: AIdent, body: Ast)                        => '{ Map(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case FlatMap(query: Ast, alias: AIdent, body: Ast)                    => '{ FlatMap(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case ConcatMap(query: Ast, alias: AIdent, body: Ast)                  => '{ ConcatMap(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case SortBy(query: Ast, alias: AIdent, criterias: Ast, ordering: Ast) => '{ SortBy(${ query.expr }, ${ alias.expr }, ${ criterias.expr }, ${ ordering.expr }) }
      case GroupBy(query: Ast, alias: AIdent, body: Ast)                    => '{ GroupBy(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case GroupByMap(query: Ast, byAlias: AIdent, byBody: Ast, mapAlias: AIdent, mapBody: Ast) =>
        '{ GroupByMap(${ query.expr }, ${ byAlias.expr }, ${ byBody.expr }, ${ mapAlias.expr }, ${ mapBody.expr }) }

      case Aggregation(operator, query)          => '{ Aggregation(${ operator.expr }, ${ query.expr }) }
      case Take(query: Ast, num: Ast)            => '{ Take(${ query.expr }, ${ num.expr }) }
      case Drop(query: Ast, num: Ast)            => '{ Drop(${ query.expr }, ${ num.expr }) }
      case Union(a, b)                           => '{ Union(${ a.expr }, ${ b.expr }) }
      case UnionAll(a, b)                        => '{ UnionAll(${ a.expr }, ${ b.expr }) }
      case Join(typ, a, b, identA, identB, body) => '{ Join(${ typ.expr }, ${ a.expr }, ${ b.expr }, ${ identA.expr }, ${ identB.expr }, ${ body.expr }) }
      case FlatJoin(typ, a, identA, on)          => '{ FlatJoin(${ typ.expr }, ${ a.expr }, ${ identA.expr }, ${ on.expr }) }
      case DistinctOn(query, alias, body)        => '{ DistinctOn(${ query.expr }, ${ alias.expr }, ${ body.expr }) }
      case Distinct(a: Ast)                      => '{ Distinct(${ a.expr }) }
      case Nested(a: Ast)                        => '{ Nested(${ a.expr }) }

  given liftAst: LiftAstSerialize[Ast] with {
    def typeTag = TType.of[Ast]
    def lift =
      case q: AQuery                                                 => liftQuery(q)
      case v: Property                                               => liftProperty(v)
      case v: AIdent                                                 => liftIdent(v)
      case v: IterableOperation                                      => liftTraversableOperation(v)
      case v: OptionOperation                                        => liftOptionOperation(v)
      case a: Assignment                                             => liftAssignment(a)
      case a: AssignmentDual                                         => liftAssignmentDual(a)
      case a: Action                                                 => liftAction(a)
      case v: Entity                                                 => liftEntity(v)
      case v: Tuple                                                  => liftTuple(v)
      case v: CaseClass                                              => liftCaseClass(v)
      case v: Ordering                                               => orderingLiftable(v)
      case Constant(ConstantValue(v), quat)                          => '{ Constant(${ ConstantExpr(v) }, ${ quat.expr }) }
      case Constant((), quat)                                        => '{ Constant((), ${ quat.expr }) }
      case Function(params: List[AIdent], body: Ast)                 => '{ Function(${ params.expr }, ${ body.expr }) }
      case FunctionApply(function: Ast, values: List[Ast])           => '{ FunctionApply(${ function.expr }, ${ values.expr }) }
      case If(cond, thenStmt, elseStmt)                              => '{ If(${ cond.expr }, ${ thenStmt.expr }, ${ elseStmt.expr }) }
      case UnaryOperation(operator: UnaryOperator, a: Ast)           => '{ UnaryOperation(${ liftOperator(operator).asInstanceOf[Expr[UnaryOperator]] }, ${ a.expr }) }
      case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${ a.expr }, ${ liftOperator(operator).asInstanceOf[Expr[BinaryOperator]] }, ${ b.expr }) }
      case v: ScalarTag                                              => liftScalarTag(v)
      case v: QuotationTag                                           => liftQuotationTag(v)
      case Infix(parts, params, pure, transparent, quat)             => '{ Infix(${ parts.expr }, ${ params.expr }, ${ pure.expr }, ${ transparent.expr }, ${ quat.expr }) }
      case OnConflict.Excluded(a)                                    => '{ OnConflict.Excluded(${ a.expr }) }
      case OnConflict.Existing(a)                                    => '{ OnConflict.Existing(${ a.expr }) }
      case NullValue                                                 => '{ NullValue }
  }

  given liftScalarTagSource: Lifters.Plain[External.Source] with
    def lift =
      case External.Source.Parser                 => '{ External.Source.Parser }
      case External.Source.UnparsedProperty(name) => '{ External.Source.UnparsedProperty(${ Expr(name) }) }

  given liftScalarTag: LiftAstSerialize[ScalarTag] with
    def typeTag = TType.of[ScalarTag]
    def lift =
      case ScalarTag(uid: String, source) => '{ ScalarTag(${ uid.expr }, ${ source.expr }) }

  given liftQuotationTag: LiftAstSerialize[QuotationTag] with
    def typeTag = TType.of[QuotationTag]
    def lift =
      case QuotationTag(uid: String) => '{ QuotationTag(${ uid.expr }) }

  given liftOperator: Lifters.Plain[Operator] with
    def lift =
      case SetOperator.contains       => '{ SetOperator.contains }
      case SetOperator.nonEmpty       => '{ SetOperator.nonEmpty }
      case SetOperator.isEmpty        => '{ SetOperator.isEmpty }
      case NumericOperator.+          => '{ NumericOperator.+ }
      case NumericOperator.-          => '{ NumericOperator.- }
      case NumericOperator.*          => '{ NumericOperator.* }
      case NumericOperator./          => '{ NumericOperator./ }
      case NumericOperator.%          => '{ NumericOperator.% }
      case NumericOperator.>          => '{ NumericOperator.> }
      case NumericOperator.<          => '{ NumericOperator.< }
      case NumericOperator.>=         => '{ NumericOperator.>= }
      case NumericOperator.<=         => '{ NumericOperator.<= }
      case StringOperator.+           => '{ StringOperator.+ }
      case StringOperator.toUpperCase => '{ StringOperator.toUpperCase }
      case StringOperator.toLowerCase => '{ StringOperator.toLowerCase }
      case StringOperator.toLong      => '{ StringOperator.toLong }
      case StringOperator.toInt       => '{ StringOperator.toInt }
      case StringOperator.startsWith  => '{ StringOperator.startsWith }
      case StringOperator.split       => '{ StringOperator.split }
      case EqualityOperator.`_==`     => '{ EqualityOperator.`_==` } // if you don't do it this way, complains about 'stable identifier error'
      case EqualityOperator.`_!=`     => '{ EqualityOperator.`_!=` } // (can't use 'ne' here because 'ne' alias is a non-stable identifier? maybe used for something else?)
      case BooleanOperator.||         => '{ BooleanOperator.|| }
      case BooleanOperator.&&         => '{ BooleanOperator.&& }
      case BooleanOperator.!          => '{ BooleanOperator.! }
}

object Lifter extends Lifters.Proxy {
  val default = new Lifter(SerializeQuat.global, SerializeAst.global)

  def NotSerializing = Lifter(SerializeQuat.None, SerializeAst.None)
  def NotSerializingAst = Lifter(SerializeQuat.global, SerializeAst.None)
  def WithBehavior(serializeQuat: Option[SerializeQuat] = None, serializeAst: Option[SerializeAst] = None) =
    Lifter(serializeQuat.getOrElse(SerializeQuat.global), serializeAst.getOrElse(SerializeAst.global))

  private[getquill] def doSerializeQuat(quat: Quat, serializeQuat: SerializeQuat) =
    serializeQuat match
      case SerializeQuat.All                                 => true
      case SerializeQuat.ByFieldCount(maxNonSerializeFields) => quat.countFields > maxNonSerializeFields
      case SerializeQuat.None                                => false
}
