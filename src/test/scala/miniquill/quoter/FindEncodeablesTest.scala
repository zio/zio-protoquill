package miniquill.quoter

import org.junit.Test
import org.junit.Assert._
import miniquill.context.mirror.Row

import io.getquill._

@main def testEncodeable = {
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  val row = new Row()
  val output = 
    FindEncodeablesUserMacro.apply[Row](List(
      ScalarValueVase("foo", "123")
    ), row)

  println(output)
}