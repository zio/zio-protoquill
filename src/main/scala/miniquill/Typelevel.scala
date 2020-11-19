package miniquill

object Typelevel {
  // trait Conv[In] { type Out; def get(in: In): Out }
  // implicit def intConvert: Conv[Int] { type Out = String } =
  //   new Conv[Int] { type Out = String; def get(i: Int): String = "str" }
  // implicit def stringConvert: Conv[String] { type Out = Int } =
  //   new Conv[String] { type Out = Int; def get(i: String): Int = 44 }

  trait Conv[In] { type Out; def get(in: In): Out }
  implicit def intConvert: Conv[Int] { type Out = String } =
    new Conv[Int] { type Out = String; def get(i: Int): String = "str" }
  implicit def stringConvert: Conv[String] { type Out = Int } =
    new Conv[String] { type Out = Int; def get(i: String): Int = 44 }

  def doConvert[T](t: T)(implicit input: Conv[T]): input.Out = input.get(t)

  def main(args: Array[String]): Unit = {
    println( doConvert("foo") )
    println( doConvert(1) )
  }
}
