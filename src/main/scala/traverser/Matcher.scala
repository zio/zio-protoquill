package traverser

import scala.quoted._

// trait Match2[T1, T2](find: Any, replace: Ast)
// val m2 = 
//   class M2[T1, T2] extends Match2[T1, T2](
//     (q: Query[T1], f:T1 => T2) => q.map(f),
//     // How to represent a function that takes any nubmer of arugments?
//     (q: Query[T1], f:T1 => T2) => Map(null, null, null)
//   )


// (q: Query[T], f: T => R) => q.map(f)
// (q: Query[T], f: T => R) => {
//   val func = parse(f)
//   Map(parse(q), func.alias, func.body)
// }

object Matcher {

  // import dotty.tools.dotc.ast.Trees.Tree
  // inline def getTree[T](op: =>T):Tree[T] = ${ getTreeImpl('op) }
  // def getTreeImpl[T: Type](op: Expr[T])(using qctx: QuoteContext): Expr[Tree[T]] = {
  //   import qctx.tasty.{_, given _}
  //   op
  // }


}