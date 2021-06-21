package io.getquill.metaprog

import java.util.Base64
import com.esotericsoftware.kryo.io.{ Input, Output }
import com.esotericsoftware.kryo.{ Kryo, Serializer }
import com.twitter.chill.{ IKryoRegistrar, KryoPool, ScalaKryoInstantiator }
import io.getquill.util.Messages
import io.getquill.quat._
import io.getquill.ast.Ast

import scala.collection.mutable
import io.getquill.ast.Visibility
import io.getquill.ast.Renameable

case class ExternalIdentSerial(name: String, quat: Quat, renameable: Renameable)
case class IdentSerial(name: String, quat: Quat, visibility: Visibility)
case class ConstantSerial(v: Any, quat: Quat)
case class OptionNoneSerial(quat: Quat)
case class EntitySerial(name: String, properties: List[io.getquill.ast.PropertyAlias], quat: Quat.Product, renameable: io.getquill.ast.Renameable)

object KryoAstSerializer {
  import scala.collection.mutable.LinkedHashMap

  // Needs to be lazy or will be initialize by ScalaJS and it doesn't exist there.
  lazy val registrar = new IKryoRegistrar {
    override def apply(k: Kryo): Unit = {
      k.register(classOf[Quat])
      k.register(classOf[Quat.Product])
      k.register(classOf[Quat.Product.Id])
      k.register(Quat.Value.getClass)
      k.register(Quat.BooleanValue.getClass)
      k.register(Quat.BooleanExpression.getClass)
      k.register(Quat.Null.getClass)
      k.register(Quat.Generic.getClass)
      k.register(Quat.Unknown.getClass)
      k.register(classOf[Quat.Product.Type])
      k.register(Quat.Product.Type.Concrete.getClass)
      k.register(Quat.Product.Type.Abstract.getClass)

      import io.getquill.ast._
      k.register(classOf[Action])
      k.register(classOf[Aggregation])
      k.register(classOf[AggregationOperator])
      k.register(AggregationOperator.getClass)
      k.register(Asc.getClass)
      k.register(AscNullsFirst.getClass)
      k.register(AscNullsLast.getClass)
      k.register(classOf[Assignment])
      k.register(classOf[Ast])
      k.register(classOf[BinaryOperation])
      k.register(classOf[BinaryOperator])
      k.register(classOf[Block])
      k.register(BooleanOperator.getClass)
      k.register(BottomTypedTerminal.getClass)
      k.register(classOf[CaseClass])
      k.register(classOf[CaseClassLift])
      k.register(classOf[CaseClassQueryLift])
      k.register(CaseClassQueryLift.getClass)
      k.register(classOf[CaseClassValueLift])
      k.register(CaseClassValueLift.getClass)
      k.register(classOf[ConcatMap])
      k.register(classOf[Delete])
      k.register(Desc.getClass)
      k.register(DescNullsFirst.getClass)
      k.register(DescNullsLast.getClass)
      k.register(classOf[Distinct])
      k.register(classOf[Drop])
      k.register(classOf[Dynamic])
      k.register(Dynamic.getClass)
      k.register(EqualityOperator.getClass)
      k.register(classOf[External])
      k.register(classOf[Filter])
      k.register(classOf[FlatJoin])
      k.register(classOf[FlatMap])
      k.register(classOf[Foreach])
      k.register(classOf[Function])
      k.register(classOf[FunctionApply])
      k.register(classOf[GroupBy])
      k.register(classOf[Ident])
      k.register(Ident.getClass)
      k.register(classOf[If])
      k.register(classOf[Infix])
      k.register(Infix.getClass)
      k.register(classOf[Insert])
      k.register(classOf[IterableOperation])
      k.register(classOf[Join])
      k.register(classOf[Lift])
      k.register(classOf[ListContains])
      k.register(classOf[Map])
      k.register(classOf[MapContains])
      k.register(classOf[Nested])
      k.register(NullValue.getClass)
      k.register(NumericOperator.getClass)
      k.register(classOf[OnConflict])
      k.register(OnConflict.getClass)
      k.register(classOf[Operation])
      k.register(classOf[Operator])
      k.register(classOf[Opinion[_]])
      k.register(classOf[OpinionValues[_]])
      k.register(classOf[OptionApply])
      k.register(classOf[OptionContains])
      k.register(classOf[OptionExists])
      k.register(classOf[OptionFlatMap])
      k.register(classOf[OptionFlatten])
      k.register(classOf[OptionForall])
      k.register(classOf[OptionGetOrElse])
      k.register(classOf[OptionGetOrNull])
      k.register(classOf[OptionIsDefined])
      k.register(classOf[OptionIsEmpty])
      k.register(classOf[OptionMap])
      k.register(OptionNoneId.getClass)
      k.register(classOf[OptionNonEmpty])
      k.register(classOf[OptionOperation])
      k.register(classOf[OptionOrNull])
      k.register(classOf[OptionSome])
      k.register(classOf[OptionTableExists])
      k.register(classOf[OptionTableFlatMap])
      k.register(classOf[OptionTableForall])
      k.register(classOf[OptionTableMap])
      k.register(classOf[Ordering])
      k.register(classOf[PostfixUnaryOperator])
      k.register(classOf[PrefixUnaryOperator])
      k.register(classOf[Property])
      k.register(Property.getClass)
      k.register(classOf[PropertyAlias])
      k.register(classOf[PropertyOrdering])
      k.register(classOf[Query])
      k.register(classOf[QuotationTag])
      k.register(classOf[QuotationTagId])
      k.register(classOf[QuotedReference])
      k.register(classOf[Renameable])
      k.register(Renameable.getClass)
      k.register(classOf[Returning])
      k.register(classOf[ReturningAction])
      k.register(ReturningAction.getClass)
      k.register(classOf[ReturningGenerated])
      k.register(classOf[ScalarLift])
      k.register(classOf[ScalarQueryLift])
      k.register(ScalarQueryLift.getClass)
      k.register(classOf[ScalarTag])
      k.register(classOf[ScalarTagId])
      k.register(classOf[ScalarValueLift])
      k.register(ScalarValueLift.getClass)
      k.register(classOf[SetContains])
      k.register(SetOperator.getClass)
      k.register(classOf[SortBy])
      k.register(StringOperator.getClass)
      k.register(classOf[Tag])
      k.register(classOf[Take])
      k.register(classOf[Terminal])
      k.register(Terminal.getClass)
      k.register(classOf[Tuple])
      k.register(classOf[TupleOrdering])
      k.register(classOf[UnaryOperation])
      k.register(classOf[UnaryOperator])
      k.register(classOf[Union])
      k.register(classOf[UnionAll])
      k.register(classOf[Update])
      k.register(classOf[Val])
      k.register(classOf[Value])
      k.register(classOf[Visibility])
      k.register(Visibility.getClass)

      k.register(classOf[Operator])
      k.register(classOf[UnaryOperator])
      k.register(classOf[PrefixUnaryOperator])
      k.register(classOf[PostfixUnaryOperator])
      k.register(classOf[BinaryOperator])
      k.register(EqualityOperator.`==`.getClass)
      k.register(EqualityOperator.`!=`.getClass)
      k.register(BooleanOperator.`!`.getClass)
      k.register(BooleanOperator.`&&`.getClass)
      k.register(BooleanOperator.`||`.getClass)
      k.register(StringOperator.`+`.getClass)
      k.register(StringOperator.`startsWith`.getClass)
      k.register(StringOperator.`split`.getClass)
      k.register(StringOperator.`toUpperCase`.getClass)
      k.register(StringOperator.`toLowerCase`.getClass)
      k.register(StringOperator.`toLong`.getClass)
      k.register(StringOperator.`toInt`.getClass)
      k.register(NumericOperator.`-`.getClass)
      k.register(NumericOperator.`+`.getClass)
      k.register(NumericOperator.`*`.getClass)
      k.register(NumericOperator.`>`.getClass)
      k.register(NumericOperator.`>=`.getClass)
      k.register(NumericOperator.`<`.getClass)
      k.register(NumericOperator.`<=`.getClass)
      k.register(NumericOperator.`/`.getClass)
      k.register(NumericOperator.`%`.getClass)
      k.register(SetOperator.`contains`.getClass)
      k.register(SetOperator.`nonEmpty`.getClass)
      k.register(SetOperator.`isEmpty`.getClass)
      k.register(AggregationOperator.getClass)
      k.register(AggregationOperator.`min`.getClass)
      k.register(AggregationOperator.`max`.getClass)
      k.register(AggregationOperator.`avg`.getClass)
      k.register(AggregationOperator.`sum`.getClass)
      k.register(AggregationOperator.`size`.getClass)


      k.register(Ident.getClass)
      k.register(ExternalIdent.getClass)
      k.register(OptionNone.getClass)
      k.register(Constant.getClass)
      k.register(Entity.getClass)

      k.register(classOf[IdentSerial])
      k.register(classOf[ExternalIdentSerial])
      k.register(classOf[ConstantSerial])
      k.register(classOf[OptionNone])
      k.register(classOf[Entity])
      k.register(classOf[EntitySerial])


      k.register(classOf[Entity],
        new Serializer[Entity]():
          override def write(kryo: Kryo, output: Output, obj: Entity): Unit =
            //println(s"## Writing Entity: ${io.getquill.util.Messages.qprint(obj)}")
            kryo.writeObject(output, EntitySerial(obj.name, obj.properties, obj.quat, obj.renameable))
          override def read(kryo: Kryo, input: Input, tpe: Class[Entity]): Entity =
            val in = kryo.readObject(input, classOf[EntitySerial])
            //println(s"## Reading Entity: ${io.getquill.util.Messages.qprint(in)}")
            Entity.Opinionated(in.name, in.properties, in.quat, in.renameable)
      )
      k.register(classOf[OptionNone],
        new Serializer[OptionNone]():
          override def write(kryo: Kryo, output: Output, obj: OptionNone): Unit =
            kryo.writeObject(output, OptionNoneSerial(obj.quat))
          override def read(kryo: Kryo, input: Input, tpe: Class[OptionNone]): OptionNone =
            val in = kryo.readObject(input, classOf[OptionNoneSerial])
            OptionNone(in.quat)
      )
      k.register(classOf[Constant],
        new Serializer[Constant]():
          override def write(kryo: Kryo, output: Output, obj: Constant): Unit =
            kryo.writeObject(output, ConstantSerial(obj.v, obj.quat))
          override def read(kryo: Kryo, input: Input, tpe: Class[Constant]): Constant =
            val in = kryo.readObject(input, classOf[ConstantSerial])
            Constant(in.v, in.quat)
      )
      k.register(classOf[Ident],
        new Serializer[Ident]():
          override def write(kryo: Kryo, output: Output, obj: Ident): Unit =
            kryo.writeObject(output, IdentSerial(obj.name, obj.quat, obj.visibility))
          override def read(kryo: Kryo, input: Input, tpe: Class[Ident]): Ident =
            val in = kryo.readObject(input, classOf[IdentSerial])
            Ident(in.name, in.quat)
      )
      k.register(classOf[ExternalIdent],
        new Serializer[ExternalIdent]():
          override def write(kryo: Kryo, output: Output, obj: ExternalIdent): Unit =
            kryo.writeObject(output, ExternalIdentSerial(obj.name, obj.quat, obj.renameable))
          override def read(kryo: Kryo, input: Input, tpe: Class[ExternalIdent]): ExternalIdent =
            val in = kryo.readObject(input, classOf[ExternalIdentSerial])
            ExternalIdent(in.name, in.quat)
      )

      // Need to make sure LinkedHashMap is converted to a list of key,value tuples before being convered
      // otherwise very strange things happen after kryo deserialization with the operation of LinkedHashMap.
      // for example, Quat.zip on LinkedHashMap would cause:
      // java.lang.ClassCastException: scala.collection.mutable.DefaultEntry cannot be cast to scala.collection.mutable.LinkedEntry
      // or even SIGSEGV on String.equals in the same place:
      // Stack: [0x00007f4c681f2000,0x00007f4c682f3000],  sp=0x00007f4c682e36e0,  free space=965k
      // Native frames: (J=compiled Java code, j=interpreted, Vv=VM code, C=native code)
      // J 15 C2 java.lang.String.equals(Ljava/lang/Object;)Z (81 bytes) @ 0x00007f4c851180da [0x00007f4c851180a0+0x3a]
      // J 5794 C2 scala.collection.immutable.Map$Map1.get(Ljava/lang/Object;)Lscala/Option; (27 bytes) @ 0x00007f4c8603c1c4 [0x00007f4c8603c040+0x184]
      // J 5794 C2 scala.collection.immutable.Map$Map1.get(Ljava/lang/Object;)Lscala/Option; (27 bytes) @ 0x00007f4c8603c1c4 [0x00007f4c8603c040+0x184]
      // J 19573 C2 io.getquill.quat.LinkedHashMapOps$.zipWith(Lscala/collection/mutable/LinkedHashMap;Lscala/collection/mutable/LinkedHashMap;Lscala/PartialFunction;)Lscala/collection/immutable/List; (49 bytes) @ 0x00007f4c860491f8 [0x00007f4c860485e0+0xc18]
      k.register(
        classOf[LinkedHashMap[String, Quat]],
        new Serializer[LinkedHashMap[String, Quat]]() {
          override def write(kryo: Kryo, output: Output, `object`: mutable.LinkedHashMap[String, Quat]): Unit = {
            kryo.writeObject(output, `object`.toList)
          }
          override def read(kryo: Kryo, input: Input, `type`: Class[mutable.LinkedHashMap[String, Quat]]): mutable.LinkedHashMap[String, Quat] = {
            val list = kryo.readObject(input, classOf[List[(String, Quat)]])
            LinkedHashMap(list: _*)
          }
        }
      )
      ()
    }
  }
  // Needs to be lazy or will be initialize by ScalaJS and it doesn't exist there.
  lazy val kryo =
    KryoPool.withByteArrayOutputStream(
      Messages.quatKryoPoolSize,
      new ScalaKryoInstantiator().withRegistrar(registrar)
    )

  def serialize(ast: Ast): String = {
    Base64.getEncoder.encodeToString(kryo.toBytesWithClass(ast))
  }

  def deserialize(str: String): Ast = {
    val bytes = Base64.getDecoder.decode(str)
    kryo.fromBytes(bytes).asInstanceOf[Ast]
  }
}
