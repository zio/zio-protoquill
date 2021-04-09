package io.getquill.util

import scala.util.{ Try, Success, Failure }
import scala.quoted._

object Format {
  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210

  object TypeOf {
    inline def apply[T]: String = ${ printTypeImpl[T] }
    def printTypeImpl[T: Type](using Quotes): Expr[String] = scala.quoted.Expr(io.getquill.util.Format.Type(scala.quoted.Type.of[T]))
  }

  object Type {
    def apply(tpe: scala.quoted.Type[_])(using Quotes) =
      import quotes.reflect._
      tpe match
        case '[tt] => Printer.TypeReprShortCode.show(TypeRepr.of[tt]) //hello
  }

  object Expr {
    def apply(expr: Expr[_])(using Quotes) =
      import quotes.reflect._
      Format(Printer.TreeShortCode.show(expr.asTerm))

    def Detail(expr: Expr[_])(using Quotes) =
      import quotes.reflect._
      Format(Printer.TreeStructure.show(expr.asTerm))
  }

  def apply(code: String) = {
      val encosedCode = 
        s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin
      
      // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
      //       use only for debugging purposes!
      def unEnclose(enclosedCode: String) = 
        val lines =
          enclosedCode
            .replaceFirst("^object DummyEnclosure \\{", "")
            .replaceFirst("\\}", "")
            .split("\n")
        val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
        // if there was a \n} on the last line, remove the }
        val linesTrimmedLast = if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
        // then if all lines had at least one indent i.e. "  " remove that
        if (linesTrimmedLast.forall(line => line.startsWith("  ")))
          linesTrimmedLast.map(line => line.replaceFirst("  ","")).mkString("\n")
        else
          linesTrimmedLast.mkString("\n")

      extension [T](t: Try[T])
        def toOptionMsg = t match
          case Success(v) => Some(v)
          case Failure(e) => None

      val formattedCode =
        for {
          cls <- Try { /* println("getting formatter"); */ Class.forName("org.scalafmt.cli.Scalafmt210") }.toOptionMsg
          inst <- Try { /* println("making instance"); */ cls.newInstance() }.toOptionMsg
          formatMethod <- Try { /* println("getting format method"); */ cls.getMethod("format", classOf[String], classOf[String]) }.toOptionMsg
          formatted <- Try { /* println("formatting"); */ formatMethod.invoke(inst, encosedCode, "Main.scala") }.toOptionMsg
        } yield String.valueOf(formatted) /* null safe way of doing .toString in scala) */

      formattedCode.map(code => /*unEnclose*/(code)).getOrElse(code)
    }
}
