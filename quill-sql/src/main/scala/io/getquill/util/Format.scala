package io.getquill.util

import scala.util.{ Try, Success, Failure }

object Format {
  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210

  def apply(code: String) = {
      val formedCode =
        s"""|object Foo {
            |  ${code}
            |}""".stripMargin

      extension [T](t: Try[T])
        def toOptionMsg = t match
          case Success(v) => println("Succeeded"); Some(v)
          case Failure(e) =>
            println("Failed: " + e.getMessage);
            e.printStackTrace
            None

      val formattedCode =
        for {
          cls <- Try { println("getting formatter"); Class.forName("org.scalafmt.cli.Scalafmt210") }.toOptionMsg
          inst <- Try { println("making instance"); cls.newInstance() }.toOptionMsg
          formatMethod <- Try { println("getting format method"); cls.getMethod("format", classOf[String], classOf[String]) }.toOptionMsg
          formatted <- Try { println("formatting"); formatMethod.invoke(inst, formedCode, "Main.scala") }.toOptionMsg
        } yield formatted

      formattedCode.getOrElse(code)
    }
}
