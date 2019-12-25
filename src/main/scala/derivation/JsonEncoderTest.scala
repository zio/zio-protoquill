package derivation

import scala.deriving._
import JsonEncoder._

case class MyPerson(name:String, age:Int, stuff: List[MyBlah])
case class MyBlah(vv: String)

object JsonEncoderTest {

  given JsonEncoder[MyPerson] = JsonEncoder.derived
  given JsonEncoder[MyBlah] = JsonEncoder.derived

  def main(args: Array[String]): Unit = {
    val encoder = summon[JsonEncoder[MyPerson]]
    println( encoder.encode(MyPerson("Joe", 123, List(MyBlah("foo"), MyBlah("bar")))) )
  } //hellooooo
}