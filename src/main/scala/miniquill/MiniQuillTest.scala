package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.QuoteDsl._

case class Address(street:String, zip:Int)
case class Person(id: Int, name: String, age: Int, address: Address)

object MiniQuillTest {
  import quoter.QueryDsl._

  def main(args: Array[String]) = {

    val pi /*: Quoted[Double] */ = quote(3.14159)

    // run(pi)
    

    // Inline def seems to work but not inline val!o
    inline def areas = quote {
      query[Person].map((p: Person) => p.address) //: EntityQuery[Double] helloooooooooooooooooooooooooooooooooooooooooooooooooooo
    }

    //println(printThenRun(""" ************************************* Areas2 ************************************* """, stuff(areas)))

    inline def areas2 = quote {
      //unquote(areas).map(a => a.zip * 3.14159)
      areas.map(a => a.zip) // * 3.14159
      //unquote(areas).map(a => a.street)oo
    }

    
    // inline def fooUse = quote {
    //   query[Person].foo()
    // }
    
    /*
      querySchema("Circle"): Playground.this.ctx.Quoted[Playground.this.ctx.EntityQuery[Playground.this.Circle]]{
        def quoted: io.getquill.ast.Entity;
        def ast: io.getquill.ast.Entity;
        def id2124864049(): Unit;
        val liftings: Object
    */

    println(run(areas2)) //helloooooooooooooooooooooooooooooooo
    // SELECT (3.14159 * c.radius) * c.radius FROM circle c
  }
}