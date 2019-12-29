package printer

import pprint.{PPrinter, Tree, Util}

import fansi.Str
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._

class AstPrinter extends pprint.Walker {

  val defaultWidth: Int = 60
  val defaultHeight: Int = Integer.MAX_VALUE
  val defaultIndent: Int = 2
  val colorLiteral: fansi.Attrs = fansi.Color.Green
  val colorApplyPrefix: fansi.Attrs = fansi.Color.Yellow

  def shouldNotPrettifyCaseClass(x: Product) = {
    val className = x.getClass.getName
    x.productArity == 0 || 
    (x.productArity == 2 && Util.isOperator(x.productPrefix)) || 
    className.startsWith(tuplePrefix) || 
    className == "scala.Some"
  }

  override def additionalHandlers: PartialFunction[Any, Tree] = {
    case x: Product if !shouldNotPrettifyCaseClass(x) =>
      val fieldMap = caseClassToMap(x)
      Tree.Apply(
        x.productPrefix,
        fieldMap.iterator.flatMap { case (k, v) =>
          val prettyValue: Tree = additionalHandlers.lift(v).getOrElse(treeify(v))
          //Seq(Tree.Infix(Tree.Literal(k), "=", prettyValue))
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

  def apply(x: Any): fansi.Str = {
    fansi.Str.join(this.tokenize(x).toSeq: _*)
  }

  def tokenize(x: Any): Iterator[fansi.Str] = {
    val tree = this.treeify(x)
    val renderer = new Renderer(defaultWidth, colorApplyPrefix, colorLiteral, defaultIndent)
    val rendered = renderer.rec(tree, 0, 0).iter
    val truncated = new Truncated(rendered, defaultWidth, defaultHeight)
    truncated
  }
}


object AstPrinter {
  val astprint = new AstPrinter()
}

val str = new AstPrinter()
def ln(str: Any) = println(new AstPrinter()(str))
