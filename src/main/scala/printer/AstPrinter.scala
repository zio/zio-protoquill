package printer

import pprint.{PPrinter, Tree, Util}

import fansi.Str
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._

class AstPrinter extends io.getquill.AstPrinter(false, false) {

  def shouldNotPrettifyCaseClass(x: Product) = {
    val className = x.getClass.getName
    x.productArity == 0 || 
    (x.productArity == 2 && Util.isOperator(x.productPrefix)) || 
    className.startsWith(tuplePrefix) || 
    className == "scala.Some"
  }

  // override def additionalHandlers: PartialFunction[Any, Tree] = {
  //   case x: Product if !shouldNotPrettifyCaseClass(x) =>
  //     val fieldMap = caseClassToMap(x)
  //     Tree.Apply(
  //       x.productPrefix,
  //       fieldMap.iterator.flatMap { case (k, v) =>
  //         val prettyValue: Tree = additionalHandlers.lift(v).getOrElse(treeify(v))
  //         //Seq(Tree.Infix(Tree.Literal(k), "=", prettyValue))
  //         Seq(prettyValue).iterator
  //       }
  //     )
  // }

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

def lnf(str: Any): Unit = 
  println(new AstPrinter()(str))

def ln(str: Any, delimiter: Option[String] = None):Unit = 
  if (System.getProperty("printAst", "false").toBoolean)
    delimiter match {
      case None => 
        println(new AstPrinter()(str))
      case Some(value) => 
        println(new AstPrinter()(str).render.split("\n").map(value + _).mkString("\n"))
    }
    
