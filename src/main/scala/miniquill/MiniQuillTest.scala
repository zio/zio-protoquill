// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.QuoteDsl._

// @main def verticalTest() = {

//   import io.getquill._
  
//   // Don't want to do 'derives' here because at this point, the specific type of GenericDecoder is not even known yet
//   // I.e. since ResultRow can be ResultSet, Row, or something else depending on the database
//   case class Address(street:String, zip:Int) extends Embedded //derives Expander
//   case class Person(id: Int, name: String, age: Int, address: Address) //derives Expander, GenericDecoder[ResultRow, ?]
  
//   inline def people = quote {
//     query[Person]
//   }
//   inline def addresses = quote {
//     people.map((p: Person) => p.address)
//   }

//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._

//   val output = run(addresses)
//   println(people.ast)
//   println(output.string)
// }


// object MiniQuillTest {

//   case class Address(street:String, zip:Int)
//   case class Person(id: Int, name: String, age: Int, address: Address)

//   def main(args: Array[String]) = {

//     //val pi /*: Quoted[Double] */ = quote(3.14159)

//     // run(pi)
//     import miniquill.quoter._
    

//     // TODO Need to implement tuples in the parser to be able to do advanced quotatation tests

//     // inline def
//     // Inline def seems to work but not inline val!o
//     val areas: Quoted[Query[Address]] = quote { // i.e. can leaf out the quote here and it'll still work
//       query[Person].map((p: Person) => p.address)
//     } //hello

//     // inline def getProperty[T, R](row: T)(f: T => R): R = {
//     //   f(row)
//     // }
//     // List(Address("123", 123)).map(element => getProperty(element)(_.zip))
//     // inline def areas2 = {
//     //   areas.map(a => getProperty(a)(_.zip)) // * 3.14159
//     // }
//     val rv = 3
//     val q = quote(rv)

//     // inline def (using val is not correct)
//     inline def areas2: Quoted[Query[Int]] = quote {
//       // areas // can't do this
//       areas.map(a => a.zip * lift(4) * q)
//     }

//     //hellooo

//     // inline def areas3 = quote {
//     //   areas2.map(a => a * lift(5))
//     // }

//     //unquote(areas).map(a => a.zip * 3.14159)
//     //unquote(areas).map(a => a.street)oo

    
//     // inline def fooUse = quote {
//     //   query[Person].foo()
//     // }
    
//     /*
//       querySchema("Circle"): Playground.this.ctx.Quoted[Playground.this.ctx.EntityQuery[Playground.this.Circle]]{
//         def quoted: io.getquill.ast.Entity;
//         def ast: io.getquill.ast.Entity;
//         def id2124864049(): Unit;
//         val liftings: Object
//     */

//     println("================= Areas2 =================")
//     printer.ln(areas2) //hello

//     println("================= Run Areas2 =================")
//     printer.ln(run(areas2)) //hellooooooooooooooooooooooooooooooooooooo
//     // SELECT (3.14159 * c.radius) * c.radius FROM circle c
//   }
// }