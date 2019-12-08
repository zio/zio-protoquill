import Miniquill._

object MiniQuillTest {
  def main(args: Array[String]) = {

    val pi /*: Quoted[Double] */ = quote(3.14159)

    // run(pi)
    case class Person(id: Int, name: String, age: Int)

    val areas = quote {
      query[Person].map((p: Person) => p.name) //: EntityQuery[Double] hello
    }

    
    /*
      querySchema("Circle"): Playground.this.ctx.Quoted[Playground.this.ctx.EntityQuery[Playground.this.Circle]]{
        def quoted: io.getquill.ast.Entity;
        def ast: io.getquill.ast.Entity;
        def id2124864049(): Unit;
        val liftings: Object
    */

    println(run(areas)) //helloooo
    // SELECT (3.14159 * c.radius) * c.radius FROM circle c
  }
}