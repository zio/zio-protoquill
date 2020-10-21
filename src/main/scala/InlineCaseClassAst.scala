import scala.quoted._
import scala.annotation.StaticAnnotation
import scala.quoted.Const

object InlineCaseClassAst {

  var counter = 0

  trait Ast
  case class Foo(child: Ast, compileId: String = "started") extends Ast
  case class Bar(child: Ast, compileId: String = "started") extends Ast
  case class Baz(compileId: String = "started") extends Ast

  inline def inspect(ast: => Ast) = ${ inspectImpl('ast) }
  def inspectImpl(using qctx: QuoteContext)(ast: Expr[Ast]): Expr[Ast] = {
    import qctx.tasty.{Type => _, _}
    counter += 1

    def compileVal = s"compile-${counter}"

    // using fooLiftable(using i: Liftable[Int], s: Liftable[String]): Liftable[Foo] {
    //   def toExpr(foo: Foo): Expr[Foo] = null
    // }

    // using fooUnliftable(using i: Unliftable[Int], s: Unliftable[String]): Unliftable[Foo] {

    // }

    // def unlift(ast: Expr[Ast]): Ast = ast match {
    //   case '{ Foo(${c}, _) } => Foo(unlift(c), compileVal)
    //   case '{ Bar(${c}, _) } => Bar(unlift(c), compileVal)
    //   case '{ Baz(_) } => Baz(compileVal)
    //   case _ => Baz("something else: " + ast.unseal.showExtractors)
    // }

    // def lift(ast: Ast): Expr[Ast] = ast match {
    //   case Baz(v) => '{ Baz(${Expr(v)}) }
    //   case Bar(c, v) => '{ Bar(${lift(c)}, ${Expr(v)}) }
    //   case Foo(c, v) => '{ Foo(${lift(c)}, ${Expr(v)}) }
    // }

    // def parse(ast: Expr[Ast]): Ast = {
    //   ast.unseal.underlyingArgument match {
    //     case Inlined(_, _, v) => parse(v.seal.cast[Ast])
    //   }
    // }

    // val unlifted = unlift(ast)
    // val relifted = lift(unlifted)
    // println(s"Ast at this point:\nseen ${ast}\nunlifted: ${unlifted}")

    // relifted

    null
  }
}