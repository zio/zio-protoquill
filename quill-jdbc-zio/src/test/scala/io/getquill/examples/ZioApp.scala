package io.getquill.examples

import io.getquill._
import io.getquill.context.ZioJdbc._
import zio.console.putStrLn
//import zio.{ App, ExitCode, URIO }

object ZioApp extends App {
  val r =
    (zio.UIO(println("a")) &>
        zio.UIO(println("b")))

  zio.Runtime.default.unsafeRun(r)
}
