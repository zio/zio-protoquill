package io.getquill.parser

import io.getquill.ast._

import boopickle.CompositePickler
import boopickle.Default._
import io.getquill.quat.Quat

import scala.reflect.classTag

import java.nio.ByteBuffer
import java.util.Base64
import scala.collection.mutable.LinkedHashMap

object AstPicklers {
  import QuatPicklers._

  // ==== Ident Picker ====
  // (if using an implicit object it must be at the top before other elements that use ident)
  implicit object identPickler extends Pickler[Ident] {
    override def pickle(value: Ident)(implicit state: PickleState): Unit = {
      state.pickle(value.name)
      state.pickle(value.bestQuat)
      state.pickle(value.visibility)(visibilityPickler)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Ident = {
      // Need to do this here because can't put things into the class by value
      val a   = state.unpickle[String]
      val b   = state.unpickle[Quat]
      val vis = state.unpickle[Visibility](visibilityPickler)
      Ident.Opinionated(a, b, vis)
    }
  }

  // ==== Entity Picker ====
  implicit object entityPickler extends Pickler[Entity]:
    override def pickle(value: Entity)(implicit state: PickleState): Unit =
      state.pickle(value.name)
      state.pickle(value.properties)
      state.pickle(
        value.quat
      ) // need to access Quat.Product, the bestQuat member is just Quat because in some cases it can be Unknown
      state.pickle(value.renameable)
      ()
    override def unpickle(implicit state: UnpickleState): Entity =
      val a = state.unpickle[String]
      val b = state.unpickle[List[PropertyAlias]]
      val c = state.unpickle[Quat.Product]
      val d = state.unpickle[Renameable]
      new Entity(a, b)(c)(d)

  implicit object distinctPickler extends Pickler[Distinct]:
    override def pickle(value: Distinct)(implicit state: PickleState): Unit =
      state.pickle(value.a)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): Distinct =
      new Distinct(state.unpickle[Ast](astPickler))

  implicit object nestedPickler extends Pickler[Nested]:
    override def pickle(value: Nested)(implicit state: PickleState): Unit =
      state.pickle(value.a)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): Nested =
      new Nested(state.unpickle[Ast](astPickler))

  implicit val queryPickler: CompositePickler[Query] =
    compositePickler[Query]
      .addConcreteType[Entity](entityPickler, classTag[Entity])
      .addConcreteType[Filter]
      .addConcreteType[Map]
      .addConcreteType[FlatMap]
      .addConcreteType[ConcatMap]
      .addConcreteType[SortBy]
      .addConcreteType[GroupBy]
      .addConcreteType[GroupByMap]
      .addConcreteType[Aggregation]
      .addConcreteType[Take]
      .addConcreteType[Drop]
      .addConcreteType[Union]
      .addConcreteType[UnionAll]
      .addConcreteType[Join]
      .addConcreteType[FlatJoin]
      .addConcreteType[DistinctOn]
      // For some reason, case classes that have only one parameter need a manually defined pickler
      .addConcreteType[Distinct](distinctPickler, classTag[Distinct])
      .addConcreteType[Nested](nestedPickler, classTag[Nested])

  // ==== Ordering Picker ====
  implicit object tupleOrderingPicker extends Pickler[TupleOrdering]:
    override def pickle(value: TupleOrdering)(implicit state: PickleState): Unit =
      state.pickle(value.elems)
      ()
    override def unpickle(implicit state: UnpickleState): TupleOrdering =
      new TupleOrdering(state.unpickle[List[Ordering]])

  implicit val propertyOrderingPickler: CompositePickler[PropertyOrdering] =
    compositePickler[PropertyOrdering]
      .addConcreteType[Asc.type]
      .addConcreteType[Desc.type]
      .addConcreteType[AscNullsFirst.type]
      .addConcreteType[DescNullsFirst.type]
      .addConcreteType[AscNullsLast.type]
      .addConcreteType[DescNullsLast.type]
  implicit val orderingPickler: CompositePickler[Ordering] =
    compositePickler[Ordering]
      .addConcreteType[TupleOrdering](tupleOrderingPicker, classTag[TupleOrdering])
      .join[PropertyOrdering](propertyOrderingPickler)

  implicit object infixPickler extends Pickler[Infix]:
    override def pickle(value: Infix)(implicit state: PickleState): Unit =
      state.pickle(value.parts)
      state.pickle(value.params)
      state.pickle(value.pure)
      state.pickle(value.transparent)
      state.pickle(value.bestQuat)
      ()
    override def unpickle(implicit state: UnpickleState): Infix =
      val a = state.unpickle[List[String]]
      val b = state.unpickle[List[Ast]]
      val c = state.unpickle[Boolean]
      val d = state.unpickle[Boolean]
      val e = state.unpickle[Quat]
      new Infix(a, b, c, d)(e)

  // ==== Function Picker ====
  implicit val functionPickler: Pickler[Function]           = generatePickler[Function]
  implicit val functionApplyPickler: Pickler[FunctionApply] = generatePickler[FunctionApply]

  // ==== ExternalIdent Picker ====
  implicit object externalIdentPickler extends Pickler[ExternalIdent] {
    override def pickle(value: ExternalIdent)(implicit state: PickleState): Unit = {
      state.pickle(value.name)
      state.pickle(value.bestQuat)
      state.pickle(value.renameable)
      ()
    }
    override def unpickle(implicit state: UnpickleState): ExternalIdent = {
      val a = state.unpickle[String]
      val b = state.unpickle[Quat]
      val c = state.unpickle[Renameable]
      ExternalIdent.Opinionated(a, b, c)
    }
  }

  // Note: Visibility and Renameable are not Ast types. They are used inside of Ast elements i.e. are auxiliary values
  // ==== Visibility Picker ====
  implicit val visibilityPickler: CompositePickler[Visibility] =
    compositePickler[Visibility]
      .addConcreteType[Visibility.Visible.type]
      .addConcreteType[Visibility.Hidden.type]
  // ==== Renameable Picker ====
  implicit val renameablePickler: CompositePickler[Renameable] =
    compositePickler[Renameable]
      .addConcreteType[Renameable.Fixed.type]
      .addConcreteType[Renameable.ByStrategy.type]

  // ==== Property Picker ====
  implicit object propertyPickler extends Pickler[Property]:
    override def pickle(value: Property)(implicit state: PickleState): Unit =
      state.pickle(value.ast)
      state.pickle(value.name)
      state.pickle(value.renameable)
      state.pickle(value.visibility)
      ()
    override def unpickle(implicit state: UnpickleState): Property =
      new Property(
        state.unpickle[Ast],
        state.unpickle[String]
      )(
        state.unpickle[Renameable],
        state.unpickle[Visibility]
      )

  // ==== OptionOperation Pickers ====
  implicit object optionNonePickler extends Pickler[OptionNone]:
    override def pickle(value: OptionNone)(implicit state: PickleState): Unit =
      val q = value.bestQuat
      state.pickle(q)
      ()
    override def unpickle(implicit state: UnpickleState): OptionNone =
      val q = state.unpickle[Quat]
      new OptionNone(q)

  implicit object optionFlattenPickler extends Pickler[OptionFlatten]:
    override def pickle(value: OptionFlatten)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionFlatten =
      new OptionFlatten(state.unpickle[Ast](astPickler))

  implicit object optionIsEmptyPickler extends Pickler[OptionIsEmpty]:
    override def pickle(value: OptionIsEmpty)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionIsEmpty =
      new OptionIsEmpty(state.unpickle[Ast](astPickler))

  implicit object optionNonEmptyPickler extends Pickler[OptionNonEmpty]:
    override def pickle(value: OptionNonEmpty)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionNonEmpty =
      new OptionNonEmpty(state.unpickle[Ast](astPickler))

  implicit object optionIsDefinedPickler extends Pickler[OptionIsDefined]:
    override def pickle(value: OptionIsDefined)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionIsDefined =
      new OptionIsDefined(state.unpickle[Ast](astPickler))

  implicit object optionSomePickler extends Pickler[OptionSome]:
    override def pickle(value: OptionSome)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionSome =
      new OptionSome(state.unpickle[Ast](astPickler))

  implicit object optionApplyPickler extends Pickler[OptionApply]:
    override def pickle(value: OptionApply)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionApply =
      new OptionApply(state.unpickle[Ast](astPickler))

  implicit object optionOrNullPickler extends Pickler[OptionOrNull]:
    override def pickle(value: OptionOrNull)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionOrNull =
      new OptionOrNull(state.unpickle[Ast](astPickler))

  implicit object optionGetOrNullPickler extends Pickler[OptionGetOrNull]:
    override def pickle(value: OptionGetOrNull)(implicit state: PickleState): Unit =
      state.pickle(value.ast)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): OptionGetOrNull =
      new OptionGetOrNull(state.unpickle[Ast](astPickler))

  implicit val optionOperationPickler: CompositePickler[OptionOperation] =
    compositePickler[OptionOperation]
      .addConcreteType[OptionFlatten](optionFlattenPickler, classTag[OptionFlatten])
      .addConcreteType[OptionGetOrElse]
      .addConcreteType[OptionFlatMap]
      .addConcreteType[OptionMap]
      .addConcreteType[OptionForall]
      .addConcreteType[OptionExists]
      .addConcreteType[OptionContains]
      .addConcreteType[OptionIsEmpty](optionIsEmptyPickler, classTag[OptionIsEmpty])
      .addConcreteType[OptionNonEmpty](optionNonEmptyPickler, classTag[OptionNonEmpty])
      .addConcreteType[OptionIsDefined](optionIsDefinedPickler, classTag[OptionIsDefined])
      .addConcreteType[OptionTableFlatMap]
      .addConcreteType[OptionTableMap]
      .addConcreteType[OptionTableExists]
      .addConcreteType[OptionTableForall]
      .addConcreteType[OptionNone](optionNonePickler, classTag[OptionNone])
      .addConcreteType[OptionSome](optionSomePickler, classTag[OptionSome])
      .addConcreteType[OptionApply](optionApplyPickler, classTag[OptionApply])
      .addConcreteType[OptionOrNull](optionOrNullPickler, classTag[OptionOrNull])
      .addConcreteType[OptionGetOrNull](optionGetOrNullPickler, classTag[OptionGetOrNull])
      .addConcreteType[FilterIfDefined]

  // ==== IterableOperation Picker ====
  implicit val iterableOperationPickler: CompositePickler[IterableOperation] =
    compositePickler[IterableOperation]
      .addConcreteType[MapContains]
      .addConcreteType[SetContains]
      .addConcreteType[ListContains]

  // ==== Operation Pickers ====
  implicit val ifPickler: Pickler[If]                         = generatePickler[If]
  implicit val assignmentPickler: Pickler[Assignment]         = generatePickler[Assignment]
  implicit val assignmentDualPickler: Pickler[AssignmentDual] = generatePickler[AssignmentDual]

  given CompositePickler[PrefixUnaryOperator] =
    compositePickler[PrefixUnaryOperator]
      .addConcreteType[BooleanOperator.`!`.type]
      .addConcreteType[NumericOperator.`-`.type]
  given CompositePickler[PostfixUnaryOperator] =
    compositePickler
      .addConcreteType[StringOperator.`toUpperCase`.type]
      .addConcreteType[StringOperator.`toLowerCase`.type]
      .addConcreteType[StringOperator.`toLong`.type]
      .addConcreteType[StringOperator.`toInt`.type]
      .addConcreteType[SetOperator.`nonEmpty`.type]
      .addConcreteType[SetOperator.`isEmpty`.type]
  given CompositePickler[UnaryOperator] =
    compositePickler
      .join[PrefixUnaryOperator]
      .join[PostfixUnaryOperator]

  given CompositePickler[AggregationOperator] =
    compositePickler
      .addConcreteType[AggregationOperator.`min`.type]
      .addConcreteType[AggregationOperator.`max`.type]
      .addConcreteType[AggregationOperator.`avg`.type]
      .addConcreteType[AggregationOperator.`sum`.type]
      .addConcreteType[AggregationOperator.`size`.type]

  given CompositePickler[BinaryOperator] =
    compositePickler
      .addConcreteType[EqualityOperator.`_==`.type]
      .addConcreteType[EqualityOperator.`_!=`.type]
      .addConcreteType[BooleanOperator.`&&`.type]
      .addConcreteType[BooleanOperator.`||`.type]
      .addConcreteType[StringOperator.`+`.type]
      .addConcreteType[StringOperator.`startsWith`.type]
      .addConcreteType[StringOperator.`split`.type]
      .addConcreteType[NumericOperator.`-`.type]
      .addConcreteType[NumericOperator.`+`.type]
      .addConcreteType[NumericOperator.`*`.type]
      .addConcreteType[NumericOperator.`>`.type]
      .addConcreteType[NumericOperator.`>=`.type]
      .addConcreteType[NumericOperator.`<`.type]
      .addConcreteType[NumericOperator.`<=`.type]
      .addConcreteType[NumericOperator.`/`.type]
      .addConcreteType[NumericOperator.`%`.type]
      .addConcreteType[SetOperator.`contains`.type]

  given CompositePickler[Operator] =
    compositePickler
      .join[UnaryOperator]
      .join[BinaryOperator]

  implicit val operationPickler: CompositePickler[Operation] =
    compositePickler
      .addConcreteType[UnaryOperation]
      .addConcreteType[BinaryOperation]
      .addConcreteType[FunctionApply]

  // ==== Value Pickers ====
  sealed trait ConstantTypes { def v: Any }
  object ConstantTypes {
    case class Int(v: scala.Int)           extends ConstantTypes
    case class Long(v: scala.Long)         extends ConstantTypes
    case class Short(v: scala.Short)       extends ConstantTypes
    case class Float(v: scala.Float)       extends ConstantTypes
    case class Double(v: scala.Double)     extends ConstantTypes
    case class Byte(v: scala.Byte)         extends ConstantTypes
    case class Boolean(v: scala.Boolean)   extends ConstantTypes
    case class String(v: java.lang.String) extends ConstantTypes
    case object Unit                       extends ConstantTypes { def v: Unit = () }
    def from(constant: Constant): ConstantTypes =
      constant.v match {
        case v: scala.Int        => ConstantTypes.Int(v)
        case v: scala.Long       => ConstantTypes.Long(v)
        case v: scala.Short      => ConstantTypes.Short(v)
        case v: scala.Float      => ConstantTypes.Float(v)
        case v: scala.Double     => ConstantTypes.Double(v)
        case v: scala.Byte       => ConstantTypes.Byte(v)
        case v: scala.Boolean    => ConstantTypes.Boolean(v)
        case v: java.lang.String => ConstantTypes.String(v)
        case v: Unit             => ConstantTypes.Unit
        case other =>
          throw new IllegalArgumentException(s"Serialization Failure: The type `${other}` is not a valid ast.Constant.")
      }
  }
  implicit object constantPickler extends Pickler[Constant] {
    implicit val constantTypesPickler: CompositePickler[ConstantTypes] =
      compositePickler[ConstantTypes]
        .addConcreteType[ConstantTypes.Int]
        .addConcreteType[ConstantTypes.Long]
        .addConcreteType[ConstantTypes.Short]
        .addConcreteType[ConstantTypes.Float]
        .addConcreteType[ConstantTypes.Double]
        .addConcreteType[ConstantTypes.Byte]
        .addConcreteType[ConstantTypes.Boolean]
        .addConcreteType[ConstantTypes.String]
        .addConcreteType[ConstantTypes.Unit.type]

    override def pickle(value: Constant)(implicit state: PickleState): Unit = {
      state.pickle(ConstantTypes.from(value))(constantTypesPickler)
      state.pickle(value.bestQuat)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Constant = {
      val a = state.unpickle(constantTypesPickler).v
      val b = state.unpickle[Quat]
      Constant(a, b)
    }
  }
  implicit object caseClassPickler extends Pickler[CaseClass]:
    override def pickle(value: CaseClass)(implicit state: PickleState): Unit =
      state.pickle(value.name)
      val map = LinkedHashMap[String, Ast]()
      value.values.foreach((k, v) => map.addOne(k, v))
      state.pickle(map)
      ()
    override def unpickle(implicit state: UnpickleState): CaseClass =
      val name     = state.unpickle[String]
      val children = state.unpickle[LinkedHashMap[String, Ast]].toList
      new CaseClass(name, children)

  // NullPointerException when this is commented out (maybe file an issue with BooPickle?)
  implicit object tuplePickler extends Pickler[Tuple]:
    override def pickle(value: Tuple)(implicit state: PickleState): Unit =
      state.pickle(value.values)
      ()
    override def unpickle(implicit state: UnpickleState): Tuple =
      new Tuple(state.unpickle[List[Ast]])

  implicit val valuePickler: CompositePickler[Value] =
    compositePickler[Value]
      .addConcreteType[Constant](constantPickler, classTag[Constant])
      .addConcreteType[NullValue.type]
      .addConcreteType[Tuple](tuplePickler, classTag[Tuple])
      .addConcreteType[CaseClass](caseClassPickler, classTag[CaseClass])

  // ==== Action Picker ====
  implicit object deletePickler extends Pickler[Delete]:
    override def pickle(value: Delete)(implicit state: PickleState): Unit =
      state.pickle(value.query)(astPickler)
      ()
    override def unpickle(implicit state: UnpickleState): Delete =
      new Delete(state.unpickle[Ast](astPickler))

  implicit val returningActionPickler: CompositePickler[ReturningAction] =
    compositePickler[ReturningAction]
      .addConcreteType[Returning]
      .addConcreteType[ReturningGenerated]

  // ========= OnConflict Picker =========
  implicit object onConflictExcludedPickler extends Pickler[OnConflict.Excluded]:
    override def pickle(value: OnConflict.Excluded)(implicit state: PickleState): Unit =
      state.pickle(value.alias)
      ()
    override def unpickle(implicit state: UnpickleState): OnConflict.Excluded =
      val id = state.unpickle[Ident]
      new OnConflict.Excluded(id)

  implicit object onConflictExistingPickler extends Pickler[OnConflict.Existing]:
    override def pickle(value: OnConflict.Existing)(implicit state: PickleState): Unit =
      state.pickle(value.alias)
      ()
    override def unpickle(implicit state: UnpickleState): OnConflict.Existing =
      val id = state.unpickle[Ident]
      new OnConflict.Existing(id)

  implicit val onConflictTargetPickler: CompositePickler[OnConflict.Target] =
    compositePickler[OnConflict.Target]
      .addConcreteType[OnConflict.NoTarget.type]
      .addConcreteType[OnConflict.Properties]
  implicit val onConflictActionPickler: CompositePickler[OnConflict.Action] =
    compositePickler[OnConflict.Action]
      .addConcreteType[OnConflict.Ignore.type]
      .addConcreteType[OnConflict.Update]

  // ==== Action Picker ====
  implicit val actionPickler: CompositePickler[Action] =
    compositePickler[Action]
      .addConcreteType[Insert]
      .addConcreteType[Update]
      .addConcreteType[Delete](deletePickler, classTag[Delete])
      .join[ReturningAction]
      .addConcreteType[Foreach]
      .addConcreteType[OnConflict]

  // Don't care about ast.Dynamic for now
  // Don't care about QuotedReference for now

  // ==== Lift Picker ====
  // Only care about the protoquill types here for now which only really cares about
  // ScalarTag and QuotationTag
  implicit val externalSourcePickler: CompositePickler[External.Source] =
    compositePickler[External.Source]
      .addConcreteType[External.Source.Parser.type]
      .addConcreteType[External.Source.UnparsedProperty]
  implicit val externalPickler: CompositePickler[Tag] =
    compositePickler[Tag]
      .addConcreteType[ScalarTag]
      .addConcreteType[QuotationTag]

  implicit val astPickler: CompositePickler[Ast] =
    compositePickler[Ast]
      .join[Query]
      .join[Ordering]
      .addConcreteType(infixPickler, classTag[Infix])
      .addConcreteType(functionPickler, classTag[Function])
      .addConcreteType(identPickler, classTag[Ident])
      .addConcreteType(externalIdentPickler, classTag[ExternalIdent])
      .addConcreteType(propertyPickler, classTag[Property])
      .join[OptionOperation]
      .join[IterableOperation]
      .addConcreteType(ifPickler, classTag[If])
      .addConcreteType[Assignment](assignmentPickler, classTag[Assignment])
      .addConcreteType[AssignmentDual](assignmentDualPickler, classTag[AssignmentDual])
      .join[Operation]
      .join[Value]
      .addConcreteType[Block]
      .addConcreteType[Val]
      .addConcreteType[OnConflict.Excluded](onConflictExcludedPickler, classTag[OnConflict.Excluded])
      .addConcreteType[OnConflict.Existing](onConflictExistingPickler, classTag[OnConflict.Existing])
      .join[Action]
      .join[Tag]
}

object BooSerializer:
  import QuatPicklers._
  import AstPicklers._
  import io.getquill.ast.{Ast => QAst}
  import io.getquill.quat.{Quat => QQuat}

  object Ast:
    def serialize(ast: QAst): String =
      val bytes            = Pickle.intoBytes(ast)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QAst =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QAst].fromBytes(ByteBuffer.wrap(bytes))

  object Quat:
    def serialize(quat: QQuat): String =
      val bytes            = Pickle.intoBytes(quat)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat].fromBytes(ByteBuffer.wrap(bytes))

  object QuatProduct:
    def serialize(product: QQuat.Product): String =
      val bytes            = Pickle.intoBytes(product)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat.Product =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat.Product].fromBytes(ByteBuffer.wrap(bytes))

end BooSerializer
