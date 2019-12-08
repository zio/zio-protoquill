import pprint.{PPrinter, Tree, Util}

import fansi.Str
import pprint.{ Renderer, Tree, Truncated }
import scala.quoted._


class ContextAstPrinter(given qctx: QuoteContext) extends AstPrinter {
  import qctx.tasty.{Ident, Tree => TTree, _}
  def iter(args: Tree*) = (args: _*).iterator

  val newHandlers: PartialFunction[Any, Tree] = {
    case id: Ident => Tree.Apply("Ident", iter(treeify("")))
    case Select(qualifier, name) => Tree.Apply("Select", iter(treeify(qualifier), treeify(name)))
  }

  val handlers = newHandlers.orElse(super.additionalHandlers)

  override def additionalHandlers: PartialFunction[Any, Tree] = handlers
}
