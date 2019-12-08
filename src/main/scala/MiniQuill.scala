import scala.quoted._
import scala.annotation.StaticAnnotation

object Miniquill {

  /*

  (q:Query[T], c: T => R) => q.map(c)
  (q:Query[T], c: T => R) =>
    val (alias, body) = parseClosure(c)
    Map(parse(q), Ident(alias.toString), parse(body))

    (one: String, two: String) => one.startsWith(two)
  */



  // DSL
  class Query[+T] {
    def map[R](f: T => R): Query[R] = new Query[R]
  }

  class EntityQuery[T] extends Query[T] // TODO can have a list of column renames?

  inline def query[T]: EntityQuery[T] = new EntityQuery

  // AST
  sealed trait Ast
  sealed trait QueryTree extends Ast
  case class Idnt(name: String) extends Ast
  case class Map(query: Ast, alias: Idnt, body: Ast) extends QueryTree
  case class Entity(name: String) extends QueryTree
  case class Property(ast: Ast, name: String) extends Ast

  sealed trait Operation extends Ast
  case class BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) extends Operation

  sealed trait Value extends Ast
  case class Const(v: Double) extends Value

  /* Operators */
  sealed trait Operator
  sealed trait BinaryOperator extends Operator
  object NumericOperator {
    case object `*` extends BinaryOperator
  }

  def lift(ast: Ast)(given qctx: QuoteContext): Expr[Ast] = ast match {
    case Const(v) => '{ Const(${Expr(v)}) }
    case Entity(name: String) => '{ Entity(${Expr(name)})  }
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${lift(query)}, ${lift(alias).asInstanceOf[Expr[Idnt]]}, ${lift(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${lift(a)}, ${lift(operator).asInstanceOf[Expr[BinaryOperator]]}, ${lift(b)})  }
    case Property(ast: Ast, name: String) => '{Property(${lift(ast)}, ${Expr(name)}) }
    case t =>
      println(t)
      qctx.error("Lifting error: " + t)
      ???
  }

  def lift(op: Operator)(given qctx: QuoteContext): Expr[Operator] = op match {
    case NumericOperator.* => '{ NumericOperator.* }
  }

  def unlift(ast: Expr[Ast])(given qctx: QuoteContext): Ast = ast match {
    case '{ Const(${b}) } =>
      import scala.quoted.matching.{Const => Constant}
      b match {
        case Constant(v: Double) => Const(v)
      }
    case '{ Entity(${b})  } =>
      import scala.quoted.matching.{Const => Constant}
      b match {
        case Constant(v: String) => Entity(v)
      }
    case '{ Idnt(${b}) } =>
      import scala.quoted.matching.{Const => Constant}
      b match {
        case Constant(v: String) => Idnt(v)
      }
    case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(unlift(query), unlift(alias).asInstanceOf[Idnt], unlift(body))
    case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(unlift(a), unlift1(operator).asInstanceOf[BinaryOperator], unlift(b))
    case '{ Property(${ast}, ${name}) } =>
      import scala.quoted.matching.{Const => Constant}
      val unname = name match {
        case Constant(v: String) => v
      }

      Property(unlift(ast), unname)
    case t =>
      println(t)
      qctx.error("Lifting error: " + t)
      ???
  }

  def unlift1(op: Expr[Operator])(given qctx: QuoteContext): Operator = op match {
    case '{ NumericOperator.* } =>  NumericOperator.*
  }

  case class Quoted[+T](val ast: Ast) {
    override def toString = ast.toString
    // maybe try parsing this in the AST which will hopefully be plugged in?
    def unquote: T = ???
  }
  inline def quote[T](body: =>T): Quoted[T] = ${ quoteImpl[T]('body) }

  class QuotedAst(ast: Ast) extends StaticAnnotation

  def quoteImpl[T: Type](body: Expr[T])(given qctx: QuoteContext): Expr[Quoted[T]] = {
    import qctx.tasty.{_, given}

    val ast = astParser(body)

    println(ast)

    val reifiedAst = lift(ast)

    '{
       Quoted[T](${reifiedAst})
    }
  }

  def astParser[T: Type](in: Expr[T])(given qctx: QuoteContext): Ast = {
    import qctx.tasty.{Type => _, _, given}
    qctx

    //println("--> " + in.unseal)
    println
    object Seal {
      def unapply[T](e: Term)(given qctx: QuoteContext) = {
        implicit val ttpe: Type[T] = e.tpe.seal.asInstanceOf[Type[T]]

        Some(e.seal.cast[T])
      }
    }

    object PossibleTypeApply {
      def unapply[T](tree: Tree): Option[Tree] =
        tree match {
          case TypeApply(inner, _) => Some(inner)
          case other => Some(other)
        }
    }

    println("================ UnderlyingArgument =================") 
    println(AstPrinter().apply( in.unseal.underlyingArgument ))
    

    in.unseal.underlyingArgument match {
      case Inlined(_, _, v) =>
        println("================ Matching Inlined =================") 
        astParser(v.seal.cast[T])
      case Literal(Constant(v: Double)) => Const(v)
      
      case 
        Typed(
          Apply(
            TypeApply(
              // Need to use showExtractors to get TypeIdent
              Select(New(TypeIdent("EntityQuery")), /* <Init> */ _), List(targ)
            ), _
          ), _
        )
        =>
        val name: String = targ.tpe.classSymbol.get.name
        Entity(name)


      case ta @ 
        Apply(
          TypeApply(
            Select(Seal(body), "map"), _
          ), 
          Lambda(valdef :: Nil, Seal(methodBody)) :: Nil
        ) =>
        //println(ta.showExtractors)
        //println(new ContextAstPrinter().apply(ta))

        println("============ Map Method ============\n" + AstPrinter().apply(ta))

        //println("================ Underlying Value Currently ================\n" + ta.show)

        val output = Map(astParser(body), Idnt(valdef.symbol.name), astParser(methodBody))
        
        println("============ Output ============\n" + output)
        output

      // case ta @ 
      //   Apply(
      //     TypeApply(
      //       Select(
      //         Typed(
      //           Apply(
      //             TypeApply(t, List(targ)), _
      //           ), _
      //         ), _
      //       ), _
      //     ), 
      //     Lambda(valdef :: Nil, Seal(body)) :: Nil
      //   ) =>

      //   println("============ Map Method ============\n" + AstPrinter().apply(ta))
      //   val name: String = targ.tpe.classSymbol.get.name
      //   Map(Entity(name), Idnt(valdef.symbol.name), astParser(body))
        
      // What does this do???
      //case Typed(t, _) => astParser(t.seal.cast[T])

      case Apply(Select(Seal(left), "*"), Seal(right) :: Nil) =>
        BinaryOperation(astParser(left), NumericOperator.*, astParser(right))

      case Apply(TypeApply(Ident("unquote"), _), List(quoted)) =>
        val rhs = quoted.symbol.tree.asInstanceOf[ValDef].rhs.get.seal

        println("=============== Unquoted RHS ================\n" + AstPrinter().apply(quoted))

        rhs match {
          case '{ new Quoted[$t]($ast) } => unlift(ast)
        }

      case Select(Seal(prefix), member) =>
        Property(astParser(prefix), member)

      case Ident(x) => Idnt(x)

      case t =>
        println("=============== Parsing Error ================\n" + AstPrinter().apply(t))
        println("=============== Extracted ================\n" + t.showExtractors)
        ???
        //println(t)
        //summon[QuoteContext].error("Parsing error: " + in.show, in)
        //???
    }
  }

  def runQuery[T](query: Quoted[Query[T]]): String = ???

  def run[T](query: Quoted[T]): String = {
    query.ast.toString
  }

  import scala.language.implicitConversions
  //inline def unquote[T](quoted: Quoted[T]): T = quoted
  // case class unquote[T](quoted: Quoted[T]) {
  //   def apply: T = ???
  // }

  def querySchema[T](entity: String): EntityQuery[T] = ???
}