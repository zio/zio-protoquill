package io.getquill.context.mirror

import scala.reflect.ClassTag
import scala.collection.mutable.LinkedHashMap

case class Row(data: (String, Any)*) {
  private lazy val dataMap = LinkedHashMap(data: _*)

  object TupleIndex {
    def unapply(str: String): Option[Int] =
      if (str.matches("_([0-9]+)"))
        Some(str.replaceFirst("_", "").toInt)
      else
        None
  }

  private def maxNumberedRow = 
    dataMap.keySet.foldLeft(0) { (currIndex, key) =>  
      key match {
        case TupleIndex(index) if (index > currIndex) => index
        case _ => currIndex
      }
    }
  private def nextNumberedRow = maxNumberedRow + 1
  private def nextTupleIndex = s"_${nextNumberedRow}"
    
  def add(value: Any) = Row((data :+ (nextTupleIndex, value)): _*)
  def add(key: String, value: Any) = Row((data :+ (key, value)): _*)

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
}

object Row {
  def fromList(data: Any*) = new Row(data.zipWithIndex.map {
    case (value, index) => (s"_${index + 1}", value)
  }: _*)

  def single(data: Any) = fromList(data)
}
