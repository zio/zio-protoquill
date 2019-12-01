object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)

    type MyType = String | List[String]
    val l: MyType = List("foo", "bar")

    val out =
    l match {
        case _:String => "string"
        case _:List[String] => "string list"
    }
    println(out)
  }

  import Logger.log

  val res =
  log("adding stuff") {
    val v = 22
    log("multing stuff") {
      val p = v * 22
      log("result is")(p)
      p
    }
    v
  }
  println(res)

  def msg = "I was compiled by dotty :)"

}

object Logger {
  var indent = 0
  inline def log[T](msg: String)(thunk: => T): T = {
    println(s"${"  " * indent} start $msg")
    indent += 1
    val result = thunk
    indent -= 1
    println(s"${"  " * indent}$msg = $result")
    result 
  }
}