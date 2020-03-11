// package miniquill.quoter

// import org.junit.Test
// import org.junit.Assert._
// import miniquill.context.mirror.Row

// import io.getquill._

// class FindEncodeablesTest {
  
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._

//   @Test
//   def simpleStringEncodeable(): Unit = {
//     val row = new Row()
//     val output = 
//       FindEncodeablesUserMacro.apply[Row](List(
//         LazyPlanter("123", "foo"), LazyPlanter("456", "bar")
//       ), row)

//     assertTrue(output match {
//       case List(("foo", Row("foo")), ("bar", Row("bar"))) => true
//       case _ => false
//     })
//   }
// }
