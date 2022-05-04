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
      val a = state.unpickle[Quat.Product.Type]
      val b = state.unpickle[LinkedHashMap[String, Quat]]
      val c = state.unpickle[LinkedHashMap[String, String]]
      Quat.Product.WithRenames(a, b, c)
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
