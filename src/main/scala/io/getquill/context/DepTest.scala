package io.getquill.context.DepTest

object DepTest {

  sealed trait MyType
  sealed trait StringType extends MyType
  sealed trait IntType extends MyType
  object StringType extends StringType
  object IntType extends IntType

  type MyElem[X] = X match {
    case StringType => String
    case IntType => Int
  }

  def myFunc[X <: MyType](v: String, mt: X): MyElem[X] = {
    mt match {
      case mt: StringType => v
      case it: IntType => v.length
    }
  }

  def main(args: Array[String]): Unit = {

    val v = myFunc("foo", StringType)
    val vv = myFunc("foo", IntType)
  }
}
