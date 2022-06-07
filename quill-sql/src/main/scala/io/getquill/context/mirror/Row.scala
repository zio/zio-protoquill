package io.getquill.context.mirror

import scala.reflect.ClassTag
import scala.collection.mutable.LinkedHashMap
import scala.annotation.targetName

object Row:
  case class Data(key: String, value: Any)
  @targetName("columns")
  def apply(values: (String, Any)*) = new Row(values.map((k, v) => Data(k, v)).toList)
  def apply(values: Any*) = new Row(values.zipWithIndex.map((v, i) => Data(s"_${i + 1}", v)).toList)
  object TupleIndex {
    def unapply(str: Any): Option[Int] =
      if (str.toString.matches("_([0-9]+)"))
        Some(str.toString.replaceFirst("_", "").toInt)
      else
        None
  }

case class Row(elements: List[Row.Data]) {
  private lazy val dataMap = LinkedHashMap(elements.map(d => (d.key, d.value)): _*)

  def data = elements.map(r => (r.key, r.value))

  def nullAt(index: Int): Boolean = data.apply(index)._2 == null

  def apply[T](index: Int)(using t: ClassTag[T]) =
    data(index)._2 match {
      case v: T  => v
      case other => throw new RuntimeException(s"Invalid column type. Expected '${t.runtimeClass}', but got '$other'")
    }

  def apply[T](key: String)(using t: ClassTag[T]) =
    dataMap(key) match {
      case v: T  => v
      case other => throw new RuntimeException(s"Invalid column type. Expected '${t.runtimeClass}', but got '$other'")
    }

  def indexOfKey(key: String) =
    val output = data.indexWhere(d => d._1 == key)
    if (output == -1) throw new IllegalArgumentException(s"Cannot find a property called '${key}'")
    output

  private def maxNumberedRow =
    dataMap.keySet.foldLeft(0) { (currIndex, key) =>
      key match {
        case Row.TupleIndex(index) if (index > currIndex) => index
        case _                                            => currIndex
      }
    }
  private def nextNumberedRow = maxNumberedRow + 1
  private def nextTupleIndex = s"_${nextNumberedRow}"

  def add(value: Any) = Row(elements :+ Row.Data(nextTupleIndex, value))
  def add(key: String, value: Any) = Row(elements :+ Row.Data(key, value))
}
