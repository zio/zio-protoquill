package io.getquill.parser

import scala.quoted.{ Type => TType, _ }
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

object SerialHelper:
  import io.getquill.quat.{ Quat => QQuat }

  def fromSerialized[T](serial: String): T = BooSerializer.Ast.deserialize(serial).asInstanceOf[T]
  def toSerialized(ast: Ast): String = BooSerializer.Ast.serialize(ast)
  object Quat:
    def fromSerialized(serial: String): QQuat = BooSerializer.Quat.deserialize(serial)
    def toSerialized(quat: QQuat): String = BooSerializer.Quat.serialize(quat)
  object QuatProduct:
    def fromSerialized(serial: String): QQuat.Product = BooSerializer.QuatProduct.deserialize(serial)
    def toSerialized(quat: QQuat.Product): String = BooSerializer.QuatProduct.serialize(quat)
end SerialHelper

trait LifterProxy {
  def default: Lifter

  def apply(ast: Ast): Quotes ?=> Expr[Ast] = default.liftableAst(ast) // can also do ast.lift but this makes some error messages simpler
  def action(ast: Action): Quotes ?=> Expr[Action] = default.liftableAction(ast)
  def assignment(ast: Assignment): Quotes ?=> Expr[Assignment] = default.liftableAssignment(ast)
  def entity(ast: Entity): Quotes ?=> Expr[Entity] = default.liftableEntity(ast)
  def tuple(ast: Tuple): Quotes ?=> Expr[Tuple] = default.liftableTuple(ast)
  def caseClass(ast: CaseClass): Quotes ?=> Expr[CaseClass] = default.liftableCaseClass(ast)
  def ident(ast: AIdent): Quotes ?=> Expr[AIdent] = default.liftableIdent(ast)
  def quat(quat: Quat): Quotes ?=> Expr[Quat] = default.liftableQuat(quat)
  def returnAction(returnAction: ReturnAction): Quotes ?=> Expr[ReturnAction] = default.liftableReturnAction(returnAction)
}

/**
 * Convert constructs of Quill Ast into Expr[Ast]. This allows them to be passed
 * back an fourth between inline Quotation blocks during compile-time which should eventually
 * be bassed into a run-call-site where they will be evaluated into SQL.
 *
 * Note that liftable List is already taken care of by the Dotty implicits
 */
case class Lifter(serializeQuat: SerializeQuat, serializeAst: SerializeAst) extends LifterProxy {
  val default = this

  extension [T](t: T)(using ToExpr[T], Quotes)
    def expr: Expr[T] = Expr(t)

  object HasSerializeBehavior:
    def unapply(value: Expr[DoSerialize])(using Quotes): Boolean =
      import quotes.reflect._
      val memberSymbol = value.asTerm.tpe.termSymbol.memberType("BehaviorType")
      value.asTerm.select(memberSymbol).tpe <:< TypeRepr.of[io.getquill.parser.SerializationBehavior.SkipSerialize]



  trait NiceLiftable[T: ClassTag] extends ToExpr[T]:
    // TODO Can we Change to 'using Quotes' without changing all the signitures? Would be simplier to extend

    def deserializeDisabled(using Quotes): Boolean =
      import quotes.reflect._
      Expr.summon[DoSerialize] match
        case Some(HasSerializeBehavior()) =>
          true
        case Some(value) =>
          false
        case _ =>
          false

    def lift: (Quotes) ?=> PartialFunction[T, Expr[T]]

    private[getquill] def liftOrThrow(element: T)(using Quotes): Expr[T] =
      import quotes.reflect._
      lift.lift(element).getOrElse {
        report.throwError(
          s"Could not Lift AST type ${classTag[T].runtimeClass.getSimpleName} from the element:\n" +
          s"${section(io.getquill.util.Messages.qprint(element).toString)}\n" +
          s"of the Quill Abstract Syntax Tree"
        )
      }

    def apply(element: T)(using Quotes): Expr[T] =
      import quotes.reflect._
      element match

        case ast: Ast if (serializeAst == SerializeAst.None) =>
          liftOrThrow(ast).asInstanceOf[Expr[T]]
        case quat: Quat if (serializeQuat == SerializeQuat.None) =>
          liftOrThrow(quat).asInstanceOf[Expr[T]]

        case ast: Ast if (deserializeDisabled) =>
          Lifter(SerializeQuat.None, SerializeAst.None).liftableAst(ast).asInstanceOf[Expr[T]]
        case quat: Quat if (deserializeDisabled) =>
          Lifter(SerializeQuat.None, SerializeAst.None).liftableQuat(quat).asInstanceOf[Expr[T]]

        case ast: Ast if (serializeAst == SerializeAst.All) =>
          tryToSerialize[Ast](ast).asInstanceOf[Expr[T]]

        case quat: Quat.Product if (Lifter.doSerializeQuat(quat, serializeQuat)) =>
          '{ SerialHelper.QuatProduct.fromSerialized(${Expr(SerialHelper.QuatProduct.toSerialized(quat))}) }.asInstanceOf[Expr[T]]
        case quat: Quat if (Lifter.doSerializeQuat(quat, serializeQuat)) =>
          '{ SerialHelper.Quat.fromSerialized(${Expr(SerialHelper.Quat.toSerialized(quat))}) }.asInstanceOf[Expr[T]]

        case _ => liftOrThrow(element)
    def unapply(t: T)(using Quotes) = Some(apply(t))

  // Technically not part of the AST this needs to be lifted in the QueryExecution and returned to the executeActionReturning context clause
  given liftableReturnAction : NiceLiftable[ReturnAction] with
    def lift =
      case ReturnAction.ReturnNothing => '{ ReturnAction.ReturnNothing }
      case ReturnAction.ReturnColumns(colums) => '{ ReturnAction.ReturnColumns(${colums.expr}) }
      case ReturnAction.ReturnRecord => '{ ReturnAction.ReturnRecord }

  given liftRenameable : NiceLiftable[Renameable] with
    def lift =
      case Renameable.ByStrategy => '{ Renameable.ByStrategy }
      case Renameable.Fixed => '{ Renameable.Fixed }

  given liftVisbility : NiceLiftable[Visibility] with {
    def lift =
      case Visibility.Visible => '{ Visibility.Visible }
      case Visibility.Hidden => '{ Visibility.Hidden }
  }

  given liftableAggregation : NiceLiftable[AggregationOperator] with {
    def lift =
      case AggregationOperator.`min`  =>  '{ AggregationOperator.`min` }
      case AggregationOperator.`max`  =>  '{ AggregationOperator.`max` }
      case AggregationOperator.`avg`  =>  '{ AggregationOperator.`avg` }
      case AggregationOperator.`sum`  =>  '{ AggregationOperator.`sum` }
      case AggregationOperator.`size` =>  '{ AggregationOperator.`size` }
  }

  given liftableProperty : NiceLiftable[Property] with {
    def lift = {
      // Don't need the other case since Property.Opinionated will match the object
      // Note: don't declare variable called 'ast' since it is used internally
      case Property.Opinionated(core: Ast, name: String, renameable: Renameable, visibility: Visibility) =>
        '{ Property.Opinionated(${core.expr}, ${name.expr}, ${renameable.expr}, ${visibility.expr}) }
    }
  }

  given liftableIdent : NiceLiftable[AIdent] with {
    def lift =
      case AIdent.Opinionated(name: String, quat, visibility) => '{ AIdent.Opinionated(${name.expr}, ${quat.expr}, ${visibility.expr})  }
  }

  given liftPropertyAlias : NiceLiftable[PropertyAlias] with {
    def lift =
      case PropertyAlias(a, b) => '{ PropertyAlias(${a.expr}, ${b.expr}) }
  }

  given liftableAssignment : NiceLiftable[Assignment] with
    def lift =
      case Assignment(ident, property, value) => '{ Assignment(${ident.expr}, ${property.expr}, ${value.expr}) }

  given liftableAssignmentDual : NiceLiftable[AssignmentDual] with
    def lift =
      case AssignmentDual(ident1, ident2, property, value) => '{ AssignmentDual(${ident1.expr}, ${ident2.expr}, ${property.expr}, ${value.expr}) }


  given liftableJoinType : NiceLiftable[JoinType] with {
    def lift =
      case InnerJoin => '{ InnerJoin }
      case LeftJoin => '{ LeftJoin }
      case RightJoin => '{ RightJoin }
      case FullJoin => '{ FullJoin }
  }

  given liftableQuatProduct: NiceLiftable[Quat.Product] with {
    def lift =
      case Quat.Product.WithRenamesCompact(tpe, fields, values, renamesFrom, renamesTo) =>
        '{ io.getquill.quat.Quat.Product.WithRenamesCompact.apply(${tpe.expr})(${fields.toList.spliceVarargs}: _*)(${values.toList.spliceVarargs}: _*)(${renamesFrom.toList.spliceVarargs}: _*)(${renamesTo.toList.spliceVarargs}: _*) }
  }

  extension [T: TType](list: List[T])(using ToExpr[T], Quotes)
    def spliceVarargs = Varargs(list.map(Expr(_)).toSeq)

  given liftableQuat : NiceLiftable[Quat] with {
    def lift =
      case Quat.Product.WithRenamesCompact(tpe, fields, values, renamesFrom, renamesTo) => '{ io.getquill.quat.Quat.Product.WithRenamesCompact.apply(${tpe.expr})(${fields.toList.spliceVarargs}: _*)(${values.toList.spliceVarargs}: _*)(${renamesFrom.toList.spliceVarargs}: _*)(${renamesTo.toList.spliceVarargs}: _*) }
      case Quat.Value => '{ io.getquill.quat.Quat.Value }
      case Quat.Null => '{ io.getquill.quat.Quat.Null }
      case Quat.Generic => '{ io.getquill.quat.Quat.Generic }
      case Quat.Unknown => '{ io.getquill.quat.Quat.Unknown }
      case Quat.BooleanValue => '{ io.getquill.quat.Quat.BooleanValue }
      case Quat.BooleanExpression => '{ io.getquill.quat.Quat.BooleanExpression }
  }

  given liftableQuatProductType: NiceLiftable[Quat.Product.Type] with {
    def lift =
      case Quat.Product.Type.Concrete => '{ io.getquill.quat.Quat.Product.Type.Concrete }
      case Quat.Product.Type.Abstract => '{ io.getquill.quat.Quat.Product.Type.Abstract }
  }

  given liftableTraversableOperation: NiceLiftable[IterableOperation] with {
    def lift =
      case MapContains(a, b) => '{ MapContains(${a.expr}, ${b.expr}) }
      case SetContains(a, b) => '{ SetContains(${a.expr}, ${b.expr}) }
      case ListContains(a, b) => '{ ListContains(${a.expr}, ${b.expr}) }
  }

  given liftableOptionOperation : NiceLiftable[OptionOperation] with {
    def lift =
      case OptionApply(a) => '{ OptionApply(${a.expr}) }
      case OptionSome(a) => '{ OptionSome(${a.expr}) }
      case OptionNone(quat) => '{ OptionNone(${quat.expr}) }
      case OptionIsEmpty(a) => '{ OptionIsEmpty(${a.expr}) }
      case OptionNonEmpty(a) => '{ OptionNonEmpty(${a.expr}) }
      case OptionIsDefined(a) => '{ OptionIsDefined(${a.expr}) }
      case OptionGetOrElse(a, b) => '{ OptionGetOrElse(${a.expr}, ${b.expr}) }
      case OptionContains(a, b) => '{ OptionContains(${a.expr}, ${b.expr}) }
      case OptionMap(a, b, c) => '{ OptionMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionFlatMap(a, b, c) => '{ OptionFlatMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionFlatten(a) => '{ OptionFlatten(${a.expr}) }
      case OptionTableMap(a, b, c) => '{ OptionTableMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionTableFlatMap(a, b, c) => '{ OptionTableFlatMap(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionExists(a, b, c) => '{ OptionExists(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionForall(a, b, c) => '{ OptionForall(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionTableExists(a, b, c) => '{ OptionTableExists(${a.expr}, ${b.expr}, ${c.expr}) }
      case OptionTableForall(a, b, c) => '{ OptionTableForall(${a.expr}, ${b.expr}, ${c.expr}) }
  }

  given liftableEntity : NiceLiftable[Entity] with
    def lift =
      //case ast if (serializeAst == SerializeAst.All) => tryToSerialize[Entity](ast)
      case Entity.Opinionated(name: String, list, quat, renameable) => '{ Entity.Opinionated(${name.expr}, ${list .expr}, ${quat.expr}, ${renameable.expr})  }

  given liftableCaseClass: NiceLiftable[CaseClass] with
    def lift =
      case CaseClass(lifts) => '{ CaseClass(${lifts.expr}) } // List lifter and tuple lifter come built in so can just do Expr(lifts) (or lifts.expr for short)

  given liftableTuple: NiceLiftable[Tuple] with
    def lift =
      case ast if (serializeAst == SerializeAst.All) => tryToSerialize[Tuple](ast)
      case Tuple(values) => '{ Tuple(${values.expr}) }

  given orderingLiftable: NiceLiftable[Ordering] with
    def lift =
      case TupleOrdering(elems) => '{ io.getquill.ast.TupleOrdering(${elems.expr}) }
      case Asc                  => '{ io.getquill.ast.Asc }
      case Desc                 => '{ io.getquill.ast.Desc }
      case AscNullsFirst        => '{ io.getquill.ast.AscNullsFirst }
      case DescNullsFirst       => '{ io.getquill.ast.DescNullsFirst }
      case AscNullsLast         => '{ io.getquill.ast.AscNullsLast }
      case DescNullsLast        => '{ io.getquill.ast.DescNullsLast }

  given liftableAction: NiceLiftable[Action] with
    def lift =
      case Insert(query: Ast, assignments: List[Assignment]) => '{ Insert(${query.expr}, ${assignments.expr}) }
      case Update(query: Ast, assignments: List[Assignment]) => '{ Update(${query.expr}, ${assignments.expr}) }
      case Delete(query: Ast) => '{ Delete(${query.expr}) }
      case Returning(action: Ast, alias: AIdent, body: Ast) => '{ Returning(${action.expr}, ${alias.expr}, ${body.expr})  }
      case ReturningGenerated(action: Ast, alias: AIdent, body: Ast) => '{ ReturningGenerated(${action.expr}, ${alias.expr}, ${body.expr})  }
      case Foreach(query: Ast, alias: AIdent, body: Ast) => '{ Foreach(${query.expr}, ${alias.expr}, ${body.expr})  }
      case OnConflict(a, b, c) => '{ OnConflict(${a.expr}, ${b.expr}, ${c.expr}) }

  given liftableConflictTarget: NiceLiftable[OnConflict.Target] with
    def lift =
      case OnConflict.NoTarget      => '{ OnConflict.NoTarget }
      case OnConflict.Properties(a) => '{ OnConflict.Properties(${a.expr}) }

  given liftableConflictAction: NiceLiftable[OnConflict.Action] with
    def lift =
      case OnConflict.Ignore    => '{ OnConflict.Ignore }
      case OnConflict.Update(a) => '{ OnConflict.Update(${a.expr}) }

  given liftableQuery: NiceLiftable[AQuery] with
    def lift =
      case e: Entity => liftableEntity(e)
      case Filter(query: Ast, alias: AIdent, body: Ast) => '{ Filter(${query.expr}, ${alias.expr}, ${body.expr})  }
      case Map(query: Ast, alias: AIdent, body: Ast) => '{ Map(${query.expr}, ${alias.expr}, ${body.expr})  }
      case FlatMap(query: Ast, alias: AIdent, body: Ast) => '{ FlatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
      case ConcatMap(query: Ast, alias: AIdent, body: Ast) => '{ ConcatMap(${query.expr}, ${alias.expr}, ${body.expr})  }
      case SortBy(query: Ast, alias: AIdent, criterias: Ast, ordering: Ast) => '{ SortBy(${query.expr}, ${alias.expr}, ${criterias.expr}, ${ordering.expr})  }
      case GroupBy(query: Ast, alias: AIdent, body: Ast) => '{ GroupBy(${query.expr}, ${alias.expr}, ${body.expr})  }
      case Aggregation(operator, query) => '{ Aggregation(${operator.expr}, ${query.expr}) }
      case Take(query: Ast, num: Ast) => '{ Take(${query.expr}, ${num.expr})}
      case Drop(query: Ast, num: Ast) => '{ Drop(${query.expr}, ${num.expr})}
      case Union(a, b) => '{ Union(${a.expr}, ${b.expr}) }
      case UnionAll(a, b) => '{ UnionAll(${a.expr}, ${b.expr}) }
      case Join(typ, a, b, identA, identB, body) => '{ Join(${typ.expr}, ${a.expr}, ${b.expr}, ${identA.expr}, ${identB.expr}, ${body.expr}) }
      case FlatJoin(typ, a, identA, on) => '{ FlatJoin(${typ.expr}, ${a.expr}, ${identA.expr}, ${on.expr}) }
      case Distinct(a: Ast) => '{ Distinct(${a.expr}) }
      case Nested(a: Ast) => '{ Nested(${a.expr}) }

  given liftableAst : NiceLiftable[Ast] with {
    def lift =
      case q: AQuery => liftableQuery(q)
      case v: Property => liftableProperty(v)
      case v: AIdent => liftableIdent(v)
      case v: IterableOperation => liftableTraversableOperation(v)
      case v: OptionOperation => liftableOptionOperation(v)
      case a: Assignment => liftableAssignment(a)
      case a: AssignmentDual => liftableAssignmentDual(a)
      case a: Action => liftableAction(a)
      case v: Entity => liftableEntity(v)
      case v: Tuple => liftableTuple(v)
      case v: CaseClass => liftableCaseClass(v)
      case v: Ordering => orderingLiftable(v)
      case Constant(ConstantValue(v), quat) => '{ Constant(${ConstantExpr(v)}, ${quat.expr}) }
      case Constant((), quat) => '{ Constant((), ${quat.expr}) }
      case Function(params: List[AIdent], body: Ast) => '{ Function(${params.expr}, ${body.expr}) }
      case FunctionApply(function: Ast, values: List[Ast]) => '{ FunctionApply(${function.expr}, ${values.expr}) }
      case If(cond, thenStmt, elseStmt) => '{ If(${cond.expr}, ${thenStmt.expr}, ${elseStmt.expr}) }
      case UnaryOperation(operator: UnaryOperator, a: Ast) => '{ UnaryOperation(${liftOperator(operator).asInstanceOf[Expr[UnaryOperator]]}, ${a.expr})  }
      case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${a.expr}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${b.expr})  }
      case ScalarTag(uid: String) => '{ScalarTag(${uid.expr})}
      case QuotationTag(uid: String) => '{QuotationTag(${uid.expr})}
      case Infix(parts, params, pure, transparent, quat) => '{ Infix(${parts.expr}, ${params.expr}, ${pure.expr}, ${transparent.expr}, ${quat.expr}) }
      case OnConflict.Excluded(a) => '{ OnConflict.Excluded(${a.expr}) }
      case OnConflict.Existing(a) => '{ OnConflict.Existing(${a.expr}) }
      case NullValue => '{ NullValue }
  }

  given liftOperator : NiceLiftable[Operator] with {
    def lift =
      case SetOperator.contains => '{ SetOperator.contains }
      case SetOperator.nonEmpty => '{ SetOperator.nonEmpty }
      case SetOperator.isEmpty => '{ SetOperator.isEmpty }
      case NumericOperator.+ => '{ NumericOperator.+ }
      case NumericOperator.- => '{ NumericOperator.- }
      case NumericOperator.* => '{ NumericOperator.* }
      case NumericOperator./ => '{ NumericOperator./ }
      case NumericOperator.% => '{ NumericOperator.% }
      case NumericOperator.> => '{ NumericOperator.> }
      case NumericOperator.< => '{ NumericOperator.< }
      case NumericOperator.>= => '{ NumericOperator.>= }
      case NumericOperator.<= => '{ NumericOperator.<= }
      case StringOperator.+ => '{ StringOperator.+ }
      case StringOperator.toUpperCase => '{ StringOperator.toUpperCase }
      case StringOperator.toLowerCase => '{ StringOperator.toLowerCase }
      case StringOperator.toLong => '{ StringOperator.toLong }
      case StringOperator.toInt => '{ StringOperator.toInt }
      case StringOperator.startsWith => '{ StringOperator.startsWith }
      case StringOperator.split => '{ StringOperator.split }
      case EqualityOperator.`_==` => '{ EqualityOperator.`_==` }    // if you don't do it this way, complains about 'stable identifier error'
      case EqualityOperator.`_!=` => '{ EqualityOperator.`_!=` }   // (can't use 'ne' here because 'ne' alias is a non-stable identifier? maybe used for something else?)
      case BooleanOperator.|| => '{ BooleanOperator.|| }
      case BooleanOperator.&& => '{ BooleanOperator.&& }
      case BooleanOperator.! => '{ BooleanOperator.! }
  }

  def tryToSerialize[T <: Ast: TType](ast: Ast)(using Quotes): Expr[T] =
    import quotes.reflect.{ Try => _, _}
    // Kryo can have issues if 'ast' is a lazy val (e.g. it will attempt to serialize Entity.Opinionated as though
    // it is an actual object which will fail because it is actually an inner class. Should look into
    // adding support for inner classes or remove them
    Try {
      val serial = SerialHelper.toSerialized(ast)
      // Needs to be casted directly in here or else the type will not be written by Expr matchers correctly
      '{ SerialHelper.fromSerialized[T](${Expr(serial)}) }
    }.recoverWith {
      case e =>
        import io.getquill.util.CommonExtensions.Throwable._
        val msg =
          s"""Could not unift-serialize the '${ast.getClass}':
             |${io.getquill.util.Messages.qprint(ast)}.
             |Performing a regular unlift instead. Due to exception:
             |${e.stackTraceToString}
             |""".stripMargin

        println(s"WARNING: ${msg}")
        quotes.reflect.report.warning(msg)
        Try(Lifter(serializeQuat, SerializeAst.None).liftableAst(ast).asInstanceOf[Expr[T]])
    }.getOrElse {
      val msg = s"Could not serialize the '${ast.getClass}' ${io.getquill.util.Messages.qprint(ast)}."
      println(s"WARNING: ${msg}")
      quotes.reflect.report.throwError(msg)
    }
}

object Lifter extends LifterProxy {
  val default = new Lifter(SerializeQuat.global, SerializeAst.global)

  def NotSerializing = Lifter(SerializeQuat.None, SerializeAst.None)
  def NotSerializingAst = Lifter(SerializeQuat.global, SerializeAst.None)
  def WithBehavior(serializeQuat: Option[SerializeQuat] = None, serializeAst: Option[SerializeAst] = None) =
    Lifter(serializeQuat.getOrElse(SerializeQuat.global), serializeAst.getOrElse(SerializeAst.global))

  private[getquill] def doSerializeQuat(quat: Quat, serializeQuat: SerializeQuat) =
    serializeQuat match
      case SerializeQuat.All => true
      case SerializeQuat.ByFieldCount(maxNonSerializeFields) => quat.countFields > maxNonSerializeFields
      case SerializeQuat.None => false
}