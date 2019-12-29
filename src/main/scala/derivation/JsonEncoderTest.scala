package derivation

import scala.deriving._
import JsonEncoder._

case class MyPerson(name:String, age:Int, stuff: List[MyBlah])
case class MyBlah(vv: String)

object JsonEncoderTest {

  val inst = MyPerson("Joe", 123, List(MyBlah("foo"), MyBlah("bar")))
  
  def main(args: Array[String]):Unit = {
    given JsonEncoder[MyPerson] = JsonEncoder.derived
    given JsonEncoder[MyBlah] = JsonEncoder.derived

    val encoder = summon[JsonEncoder[MyPerson]]
    println( encoder.encode(inst) )
  }
}