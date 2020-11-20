package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest5_TypelevelAdv {

  import io.getquill._
  case class Address(street: String, zip: Int, fk: Int) extends Embedded //helloooo
  given Embedable[Address] //hello
  case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Stock(id: Int, symbol: String)
  case class Option(id: Int, stockFk: Int, symbol: String)
  case class Series(id: Int, optionFk: Int, effective: Int, expiration: Int)

  trait Path[From, To]:
    type Out
    inline def get: Out
  
  class PathFromStockToOption extends Path[Stock, Option]:
    type Out = Query[(Stock, Option)]
    inline def get: Query[(Stock, Option)] =
      for {
        s <- query[Stock]
        o <- query[Option].join(o => o.stockFk == s.id)
      } yield (s, o)
  
  class PathFromStockToSeries extends Path[Stock, Series]:
    type Out = Query[(Stock, Option, Series)]
    inline def get: Query[(Stock, Option, Series)] =
      for {
        s <- query[Stock]
        o <- query[Option].join(o => o.stockFk == s.id)
        r <- query[Series].join(r => r.optionFk == o.id)
      } yield (s, o, r)
  
  inline given pathFromStockToOption as PathFromStockToOption = new PathFromStockToOption
  inline given pathFromStockToSeries as PathFromStockToSeries = new PathFromStockToSeries

  inline def path[F, T](using inline path: Path[F, T]): path.Out = path.get
  
  // inline def q1 = quote { path[Stock, Option].filter(so => so._1.symbol == "MSFT") }
  // println( run(q1) )

  inline def q1 = quote { path[Stock, Series].filter(so => so._1.symbol == "MSFT") }
  println( run(q1) )




  def main(args: Array[String]): Unit = { //hellooooooo


  }
}
