package miniquill.context.mirror

import scala.reflect.ClassTag

case class Row(data: Any*) {
  def add(value: Any) = Row((data :+ value): _*)
  def apply[T](index: Int)(given t: ClassTag[T]) =
    data(index) match {
      case v: T  => v
      case other => throw new RuntimeException(s"Invalid column type. Expected '${t.runtimeClass}', but got '$other'")
    }
}