// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.Dsl._
// import scala.compiletime.{erasedValue, summonFrom, constValue}
// import miniquill.quoter.QueryDsl._

// object InlineMacroTest1FunctionalDerivation {
//   import io.getquill._
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._
//   import io.getquill.derived.MapProc

//   case class Person(firstName: String, lastName: String, age: Int)

//   val values: Map[String, String] = Map("firstName" -> "Joe", "age" -> "22")  

//   inline def q = quote {
//     query[Person].filter(p => 
//       MapProc(p, (a, b) => lift(values.get(b).getOrElse("%")) == a )
//     )
//   }

//   println( run(q).string )
  
// }
