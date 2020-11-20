package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._

object InlineMacroTest5_TypelevelAdvRow {

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
    inline def get(inline from: From): Out
  
  class PathFromStockToOption extends Path[Stock, Option]:
    type Out = Query[Option]
    inline def get(inline from: Stock): Query[Option] =
      for {
        o <- query[Option].join(o => o.stockFk == from.id)
      } yield o
  
  class PathFromStockToSeries extends Path[Stock, Series]:
    type Out = Query[(Option, Series)]
    inline def get(inline from: Stock): Query[(Option, Series)] =
      for {
        o <- query[Option].join(o => o.stockFk == from.id)
        r <- query[Series].join(r => r.optionFk == o.id)
      } yield (o, r)
  
  inline given pathFromStockToOption as PathFromStockToOption = new PathFromStockToOption
  inline given pathFromStockToSeries as PathFromStockToSeries = new PathFromStockToSeries

  inline def path[F, T](inline from: F)(using inline path: Path[F, T]): path.Out = 
    path.get(from)
  
  // inline def q1 = quote { path[Stock, Option].filter(so => so._1.symbol == "MSFT") }
  // println( run(q1) )

  inline def q1 = quote { 
    for {
      p <- query[Stock] 
      or <- path[Stock, Series](p)
    } yield (p, or)
  }
  println( run(q1) )




  def main(args: Array[String]): Unit = { //hellooooooooo


  }
}
