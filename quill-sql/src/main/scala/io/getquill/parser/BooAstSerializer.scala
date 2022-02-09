package io.getquill.parser

import io.getquill.ast._

import boopickle.CompositePickler
import boopickle.Default._
import io.getquill.quat.Quat

import scala.reflect.classTag

import java.nio.ByteBuffer
import java.util.Base64
import scala.collection.mutable.LinkedHashMap

// TODO Not sure why need to copy this from the quill-engine module. Maybe because it is only compiled for JS? Should investigate
object QuatPicklers {
  implicit object productPickler extends Pickler[Quat.Product] {
    override def pickle(value: Quat.Product)(implicit state: PickleState): Unit = {
      state.pickle(value.tpe)
      state.pickle(value.fields)
      state.pickle(value.renames)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Quat.Product = {
      Quat.Product.WithRenames(
        state.unpickle[Quat.Product.Type],
        state.unpickle[LinkedHashMap[String, Quat]],
        state.unpickle[LinkedHashMap[String, String]]
      )
    }
  }

  implicit val quatProductTypePickler: Pickler[Quat.Product.Type] =
    compositePickler[Quat.Product.Type]
      .addConcreteType[Quat.Product.Type.Concrete.type]
      .addConcreteType[Quat.Product.Type.Abstract.type]

  implicit val quatProductPickler: Pickler[Quat] =
    compositePickler[Quat]
      .addConcreteType[Quat.Product](productPickler, scala.reflect.classTag[Quat.Product])
      .addConcreteType[Quat.Generic.type]
      .addConcreteType[Quat.Unknown.type]
      .addConcreteType[Quat.Value.type]
      .addConcreteType[Quat.BooleanValue.type]
      .addConcreteType[Quat.BooleanExpression.type]
      .addConcreteType[Quat.Null.type]
}

object AstPicklers {
  import QuatPicklers._

  // ==== Ident Picker ====
  // (if using an implicit object it must be at the top before other elements that use ident)
  implicit object identPickler extends Pickler[Ident] {
    override def pickle(value: Ident)(implicit state: PickleState): Unit = {
      state.pickle(value.name)
      state.pickle(value.bestQuat)
      state.pickle(value.visibility)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Ident = {
      Ident.Opinionated(state.unpickle[String], state.unpickle[Quat], state.unpickle[Visibility])
    }
  }

  // ==== Entity Picker ====
  implicit object entityPickler extends Pickler[Entity]:
    override def pickle(value: Entity)(implicit state: PickleState): Unit =
      state.pickle(value.name)
      state.pickle(value.properties)
      state.pickle(value.bestQuat)
      state.pickle(value.renameable)
      ()
    override def unpickle(implicit state: UnpickleState): Entity =
      new Entity(state.unpickle[String], state.unpickle[List[PropertyAlias]])(state.unpickle[Quat.Product])(state.unpickle[Renameable])

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
      .addConcreteType[Aggregation]
      .addConcreteType[Take]
      .addConcreteType[Drop]
      .addConcreteType[Union]
      .addConcreteType[UnionAll]
      .addConcreteType[Join]
      .addConcreteType[FlatJoin]
      .addConcreteType[Distinct](distinctPickler, classTag[Distinct])
      .addConcreteType[Nested](nestedPickler, classTag[Nested])

  // ==== Ordering Picker ====
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
      .addConcreteType[TupleOrdering]
      .join[PropertyOrdering](propertyOrderingPickler)

  implicit object infixPickler extends Pickler[Infix] {
    override def pickle(value: Infix)(implicit state: PickleState): Unit = {
      state.pickle(value.parts)
      state.pickle(value.params)
      state.pickle(value.pure)
      state.pickle(value.transparent)
      state.pickle(value.bestQuat)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Infix = {
      new Infix(
        state.unpickle[List[String]],
        state.unpickle[List[Ast]],
        state.unpickle[Boolean],
        state.unpickle[Boolean]
      )(
        state.unpickle[Quat]
      )
    }
  }

  // ==== Function Picker ====
  implicit val functionPickler: Pickler[Function] = generatePickler[Function]
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
      ExternalIdent.Opinionated(state.unpickle[String], state.unpickle[Quat], state.unpickle[Renameable])
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

  implicit val propertyPickler: Pickler[Property] = generatePickler[Property]

  // ==== OptionOperation Pickers ====
  implicit object optionNonePickler extends Pickler[OptionNone]:
    override def pickle(value: OptionNone)(implicit state: PickleState): Unit =
      state.pickle(value.bestQuat)
      ()
    override def unpickle(implicit state: UnpickleState): OptionNone =
      new OptionNone(state.unpickle[Quat])

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


  // ==== IterableOperation Picker ====
  implicit val iterableOperationPickler: CompositePickler[IterableOperation] =
    compositePickler[IterableOperation]
      .addConcreteType[MapContains]
      .addConcreteType[SetContains]
      .addConcreteType[ListContains]

  // ==== Operation Pickers ====
  implicit val ifPickler: Pickler[If] = generatePickler[If]
  implicit val assignmentPickler: Pickler[Assignment] = generatePickler[Assignment]
  implicit val assignmentDualPickler: Pickler[AssignmentDual] = generatePickler[AssignmentDual]
  implicit val unaryOperatorPickler: CompositePickler[UnaryOperator] =
    compositePickler[UnaryOperator]
      .addConcreteType[PrefixUnaryOperator](generatePickler[PrefixUnaryOperator], classTag[PrefixUnaryOperator])
      .addConcreteType[PostfixUnaryOperator](generatePickler[PostfixUnaryOperator], classTag[PostfixUnaryOperator])
  implicit val binaryOperatorPicker: Pickler[BinaryOperator] = generatePickler[BinaryOperator]
  implicit val operatorPickler: Pickler[Operator] =
    compositePickler[Operator]
      .join[UnaryOperator]
      .addConcreteType[BinaryOperator](binaryOperatorPicker, classTag[BinaryOperator])
  implicit val operationPickler: CompositePickler[Operation] =
    compositePickler[Operation]
      .addConcreteType[UnaryOperation]
      .addConcreteType[BinaryOperation]
      .addConcreteType[FunctionApply]


  // ==== Value Pickers ====
  sealed trait ConstantTypes { def v: Any }
  object ConstantTypes {
    case class Int(v: scala.Int) extends ConstantTypes
    case class Long(v: scala.Long) extends ConstantTypes
    case class Short(v: scala.Short) extends ConstantTypes
    case class Float(v: scala.Float) extends ConstantTypes
    case class Double(v: scala.Double) extends ConstantTypes
    case class Byte(v: scala.Byte) extends ConstantTypes
    case class Boolean(v: scala.Boolean) extends ConstantTypes
    case class String(v: java.lang.String) extends ConstantTypes
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

    override def pickle(value: Constant)(implicit state: PickleState): Unit = {
      state.pickle(ConstantTypes.from(value))(constantTypesPickler)
      state.pickle(value.bestQuat)
      ()
    }
    override def unpickle(implicit state: UnpickleState): Constant = {
      Constant(state.unpickle(constantTypesPickler).v, state.unpickle[Quat])
    }
  }
  implicit object nullValuePicker extends Pickler[NullValue.type] {
    case object NullValueFlag
    override def pickle(value: NullValue.type)(implicit state: PickleState): Unit = {
      state.pickle(NullValueFlag)
      ()
    }
    override def unpickle(implicit state: UnpickleState): NullValue.type =
      NullValue
  }
  implicit val valuePickler: CompositePickler[Value] =
    compositePickler[Value]
      .addConcreteType[Constant](constantPickler, classTag[Constant])
      .addConcreteType(nullValuePicker, classTag[NullValue.type])
      .addConcreteType[Tuple]
      .addConcreteType[CaseClass]


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
      .addConcreteType(assignmentPickler, classTag[Assignment])
      .addConcreteType(assignmentDualPickler, classTag[AssignmentDual])
      .join[Operation]
      .join[Value]
      .addConcreteType[Block]
      .addConcreteType[Val]
      .join[Action]
      .join[Tag]
}


object BooSerializer:
  import QuatPicklers._
  import AstPicklers._
  import io.getquill.ast.{ Ast => QAst }
  import io.getquill.quat.{ Quat => QQuat }

  object Ast:
    def serialize(ast: QAst): String =
      val bytes = Pickle.intoBytes(ast)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QAst =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QAst].fromBytes(ByteBuffer.wrap(bytes))

  object Quat:
    def serialize(quat: QQuat): String =
      val bytes = Pickle.intoBytes(quat)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat].fromBytes(ByteBuffer.wrap(bytes))

  object QuatProduct:
    def serialize(product: QQuat.Product): String =
      val bytes = Pickle.intoBytes(product)
      val arr: Array[Byte] = new Array[Byte](bytes.remaining())
      bytes.get(arr)
      Base64.getEncoder.encodeToString(arr)
    def deserialize(str: String): QQuat.Product =
      val bytes = Base64.getDecoder.decode(str)
      Unpickle[QQuat.Product].fromBytes(ByteBuffer.wrap(bytes))

end BooSerializer
