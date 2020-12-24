// package io.getquill.quoter

// import io.getquill.context.mirror.Row
// import io.getquill._
// import org.scalatest._

// class FindEncodeablesTest extends Spec with Inside {
  
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._

//   "Locate and apply encodeables" in {
//     val row = new Row()
//     val output = 
//       FindEncodeablesUserMacro.apply[Row](List(
//         LazyPlanter("123", "foo"), LazyPlanter("456", "bar")
//       ), row)

//     inside(output) {
//       case List(("foo", Row("foo")), ("bar", Row("bar"))) =>
//     }
//   }
// }
