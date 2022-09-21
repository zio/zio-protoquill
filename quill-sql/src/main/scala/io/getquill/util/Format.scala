package io.getquill.util

import scala.util.{Try, Success, Failure}
import scala.quoted._
import io.getquill.util.ProtoMessages
import io.getquill.metaprog.DeserializeAstInstances
import io.getquill.Quoted

object Format {
  // import org.scalafmt.interfaces.Scalafmt
  // import org.scalafmt.cli.Scalafmt210
  object TypeOf {
    def apply[T: Type](using Quotes) =
      import quotes.reflect._
      Format.Type(summon[Type[T]])
  }

  object TypeRepr {
    // since we need a qctx to be able to actually pass in a TypeRepr element, it's not possible
    // (unless lampepfl/dotty#10689 is resolved) to create a global module that does TypeRepr formatting. This is a bit
    // of a hacky way around that that just requires the element to be an inner class of a Quotes instance
    // and the casts it to the specific Quotes insance. Should reconsider this when lampepfl/dotty#10689 is fixed.
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr])
  }

  object Term:
    def apply(term: Quotes#reflectModule#Term)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TreeShortCode.show(term.asInstanceOf[qctx.reflect.Term])

  object TermRaw:
    def apply(term: Quotes#reflectModule#Term)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TreeStructure.show(term.asInstanceOf[qctx.reflect.Term])

  object Tree:
    def apply(tree: Quotes#reflectModule#Tree)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TreeShortCode.show(tree.asInstanceOf[qctx.reflect.Tree])

  /** Same as TypeRepr but also widens the type since frequently types are singleton i.e. 'person.name' has the type 'name' as opposed to String */
  object TypeReprW {
    def apply(typeRepr: Quotes#reflectModule#TypeRepr)(using qctx: Quotes) =
      import qctx.reflect._
      Printer.TypeReprShortCode.show(typeRepr.asInstanceOf[qctx.reflect.TypeRepr].widen)
  }

  object Type {
    def apply(tpe: scala.quoted.Type[_])(using Quotes) =
      import quotes.reflect._
      tpe match
        case '[tt] => Printer.TypeReprShortCode.show(quotes.reflect.TypeRepr.of[tt])
        case _     => tpe
  }

  object QuotedExpr {
    import io.getquill.metaprog.{QuotationLotExpr, Uprootable, Pluckable, Pointable}
    import io.getquill.parser.Unlifter

    def apply(expr: Expr[Quoted[_]])(using Quotes) =
      expr match
        case QuotationLotExpr(quotationLot) =>
          quotationLot match {
            case Uprootable(uid, astTree, inlineLifts) =>
              case class Quoted(ast: io.getquill.ast.Ast, lifts: List[String], runtimeQuotes: String = "Nil")
              val ast = Unlifter(astTree)
              io.getquill.util.Messages.qprint(Quoted(ast, inlineLifts.map(l => Format.Expr(l.plant))))
            case Pluckable(uid, astTree, _) => Format.Expr(expr)
            case other                      => Format.Expr(expr)
          }
        case _ => Format.Expr(expr)
  }

  object Expr {
    def apply(expr: Expr[_], showErrorTrace: Boolean = false)(using Quotes) =
      import quotes.reflect._
      val deserExpr = DeserializeAstInstances(expr)
      Format(Printer.TreeShortCode.show(deserExpr.asTerm), showErrorTrace)

    def Detail(expr: Expr[_])(using Quotes) =
      import quotes.reflect._
      val term = expr.asTerm
      if (ProtoMessages.errorDetail) {
        s"""|
            |s"==== Expression ====
            |  ${Format(Printer.TreeShortCode.show(term))}
            |==== Extractors ===
            |  ${Format(Printer.TreeStructure.show(term))}
            |""".stripMargin
      } else {
        Format(Printer.TreeShortCode.show(term))
      }
  }

  def apply(code: String, showErrorTrace: Boolean = false) = {
    val enclosedCode =
      s"""|object DummyEnclosure {
            |  ${code}
            |}""".stripMargin

    // NOTE: Very ineffifient way to get rid of DummyEnclosure on large blocks of code
    //       use only for debugging purposes!
    def unEnclose(enclosedCode: String) =
      val lines =
        enclosedCode
          .replaceFirst("^object DummyEnclosure \\{", "")
          .reverse
          .replaceFirst("\\}", "")
          .reverse
          .split("\n")
      val linesTrimmedFirst = if (lines.head == "") lines.drop(1) else lines
      // if there was a \n} on the last line, remove the }
      val linesTrimmedLast = if (linesTrimmedFirst.last == "") linesTrimmedFirst.dropRight(1) else linesTrimmedFirst
      // then if all lines had at least one indent i.e. "  " remove that
      if (linesTrimmedLast.forall(line => line.startsWith("  ")))
        linesTrimmedLast.map(line => line.replaceFirst("  ", "")).mkString("\n")
      else
        linesTrimmedLast.mkString("\n")

    val formatted =
      Try {
        // val formatCls = classOf[ScalafmtFormat.type]
        // val result = formatCls.getMethod("apply").invoke(null, enclosedCode)
        // println("============ GOT HERE ===========")
        // val resultStr = s"${result}"
        // resultStr
        ScalafmtFormat(
          // Various other cleanup needed to make the formatter happy
          enclosedCode
            .replace("_*", "_")
            .replace("_==", "==")
            .replace("_!=", "!="),
          showErrorTrace
        )
      }.getOrElse {
        println("====== WARNING: Scalafmt Not Detected ====")
        enclosedCode
      }

    unEnclose(formatted)
  }
}
