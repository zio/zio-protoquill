package io.getquill.util.printer

import pprint.{PPrinter, Tree, Util}

import fansi.Str
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._
import io.getquill.util.FromMessages

class AstPrinter extends io.getquill.AstPrinter(false, false, FromMessages.traceQuats) {

  val removePrefixes = Set(

  )

  def shouldAddProperties(x: Product) = {
    val className = x.getClass.getName
    (x.productArity > 2) && //&& Util.isOperator(x.productPrefix) 
    (removePrefixes.filter(prefix => className.startsWith(prefix)).isEmpty)
  }

  override def additionalHandlers: PartialFunction[Any, Tree] = {
    case x: Product if shouldAddProperties(x) =>
      val fieldMap = caseClassToMap(x)
      Tree.Apply(
        x.productPrefix,
        fieldMap.iterator.flatMap { case (k, v) =>
          val prettyValue: Tree = additionalHandlers.lift(v).getOrElse(treeify(v))
          if (System.getProperty("quill.ast.props", "false") == "true")
            Seq(Tree.Infix(Tree.Literal(k), "=", prettyValue))
          else
            Seq(prettyValue).iterator
        }
      )
  }

  protected def caseClassToMap(cc: Product): Map[String, Any] = {
    val fieldValues = cc.productIterator.toSet
    val fields = cc.getClass.getDeclaredFields.toSeq
      .filterNot(f => f.isSynthetic || java.lang.reflect.Modifier.isStatic(f.getModifiers))
    fields.map { f =>
      f.setAccessible(true)
      f.getName -> f.get(cc)
    }.filter { case (k, v) => fieldValues.contains(v) }
      .toMap
  }
}


object AstPrinter {
  val astprint = new AstPrinter()
}

def str(str: Any) =
  new AstPrinter()(str).render

def lnfbw(str: Any): Unit = 
  println(new AstPrinter()(str).plainText)

def lnf(str: Any): Unit = 
  println(new AstPrinter()(str))

def unapplier(message: String) = new Unapplier(message)
class Unapplier(message: String) {
  def unapply[T](t: T): Option[T] = 
    println(s"***************** [[[[[${message}]]]]]] *****************")
    println(s"Encountered:\n${t.toString.split("\n").map("  " + _).mkString("\n")}")
    Some(t)
}


def xunapplier(message: String)(using Quotes) = new XUnapplier(message)
class XUnapplier(message: String)(using Quotes) {
  import quotes.reflect._
  def unapply[T](t: T): Option[T] = 
    println(s"***************** [[[[[${message}]]]]]] *****************")
    def tString = 
      t match {
        case e: Expr[_] => e.show
        case Some(e: Expr[_]) => e.show
        case t: Term => str(t)
        case other => other.toString
      }

    def tStringAst = 
      t match {
        case e: Expr[_] => str(e.asTerm)
        case Some(e: Expr[_]) => str(e.asTerm)
        case t: Term => str(t)
        case other => other.toString
      }

    println(s"Encountered:\n${tString.split("\n").map("  " + _).mkString("\n")}")
    println(tStringAst)
    Some(t)
}

def ln(str: Any, delimiter: Option[String] = None):Unit = 
  if (System.getProperty("printAst", "false").toBoolean)
    delimiter match {
      case None => 
        println(new AstPrinter()(str))
      case Some(value) => 
        println(new AstPrinter()(str).render.split("\n").map(value + _).mkString("\n"))
    }
    
