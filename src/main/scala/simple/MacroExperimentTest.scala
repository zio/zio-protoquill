// package simple

// import miniquill.quoter.CoreDsl
// import simple.MacroExperiment._

// // @main def testStuff() = {

// //   println("******** Reg Stuff *******")
// //   printTree(reg_stuff)

// //   println("******** Stuff *******")
// //   printTree(stuff)

  
// // }

// object FunObject {
//   def fun[T](t: T => String) = t
//   def funString(t: String => String) = t
// }

// case class Age(value: Int)
// case class MyPerson(name: String, age: Option[Age])

// @main def testOtherstuff() = { //hello

//   // Step 6
//   //import miniquill.quoter.CoreDsl._
//   import miniquill.quoter.CoreDsl
//   // import ShellObj._

//   printTree {
//   //   //querySchema[MyPerson]("personTable", _.name -> "tableNameColumn")
//   //   //FunObject.fun((x: String) => "blah")
//   //   //(x: String) => x.length -> "len" // Step 1, 2
//   //   (x: String) => "blahblah" // Step 3, 4, 5
//     CoreDsl.querySchema[MyPerson]("personTbl", _.name -> "blah", _.age.map(_.value) -> "blaaah") //hello
//   //   //colVar[MyPerson]("blah", _.name -> "bar", _.age -> "blin")
//   //   //colVar[MyPerson]("blah")
//   //   // colVar("blah")
//   }
// }
