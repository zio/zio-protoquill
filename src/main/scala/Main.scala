object Main {

  def main(args: Array[String]): Unit = {
    
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