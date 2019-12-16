package miniquill

import miniquill.MiniQuill._
import simple.SimpleMacro._

import scala.language.implicitConversions

object MiniQuillTest {
  def main(args: Array[String]) = {

    val pi /*: Quoted[Double] */ = quote(3.14159)

    // run(pi)
    case class Address(street:String, zip:Int)
    case class Person(id: Int, name: String, age: Int, address: Address)

    // Inline def seems to work but not inline val!o
    inline def areas = quote {
      query[Person].map((p: Person) => p.address) //: EntityQuery[Double] helloo
    }

    //println(printThenRun(""" ************************************* Areas2 ************************************* """, stuff(areas)))

    inline def areas2 = quote {
      //unquote(areas).map(a => a.zip * 3.14159)
      areas.map(a => a.zip * 3.14159)
      //unquote(areas).map(a => a.street)oo
    }

    

    
    /*
      querySchema("Circle"): Playground.this.ctx.Quoted[Playground.this.ctx.EntityQuery[Playground.this.Circle]]{
        def quoted: io.getquill.ast.Entity;
        def ast: io.getquill.ast.Entity;
        def id2124864049(): Unit;
        val liftings: Object
    */

    println(run(areas2)) //helloooo
    // SELECT (3.14159 * c.radius) * c.radius FROM circle c
  }
}