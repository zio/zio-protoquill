
package miniquill.parser

import miniquill.ast._
import miniquill.quoter._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import scala.deriving._

object Parser {
  import QueryDsl._

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






  def astParser[T: Type](in: Expr[T])(given qctx: QuoteContext): Ast = {
    import qctx.tasty.{Type => _, _, given}
    import printer.ContextAstPrinter._

    //println(in.show)
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

    println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WHOLE TREE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    println(AstPrinter.astprint(in.unseal.underlyingArgument).render.split("\n").map("%% " + _).mkString("\n"))
    
    // in.unseal.underlyingArgument.seal
    in.unseal.underlyingArgument.seal match {
      
      // Can't do existential types e.g. Query[_] as of 0.21.0-RC1. Need to make variable for the types e.g. $qt
      case vv @ '{ ($k:Query[$qt]).map[$mt]($l) } => 
        println(s"************ MATCHES ${AstPrinter.astprint(vv.unseal.underlyingArgument)} **********")

      //case vv @ '{ (${k}:Query[_]) } => // Does not match with Any
      //   println(s"************ MATCHES Entity ${AstPrinter.astprint(vv.unseal.underlyingArgument)} **********")
        

      case _ => 
        println(s"************ Does not match k **********")
    }

    in.unseal.underlyingArgument match {
      case Inlined(_, _, v) =>
        println("Case Inlined")
        astParser(v.seal.cast[T])

        // TODO Need to figure how how to do with other datatypes
      case Literal(Constant(v: Double)) => 
        println("Case Literal Constant")
        Const(v)
      
      case 
        Typed(
          Apply(
            TypeApply(
              // Need to use showExtractors to get TypeIdentt
              Select(New(TypeIdent("EntityQuery")), /* <Init> */ _), List(targ)
            ), _
          ), _
        )
        =>
        println("Typed Apply of EntityQuery")
        val name: String = targ.tpe.classSymbol.get.name
        Entity(name)


      case ta @ 
        Apply(
          TypeApply(
            Select(Seal(body), "map"), _
          ), 
          Lambda(valdef :: Nil, Seal(methodBody)) :: Nil
        ) =>

        println("============ Case Map Method ============\n" + AstPrinter().apply(ta))
        val output = Map(astParser(body), Idnt(valdef.symbol.name), astParser(methodBody))
        output

      // When just doing .unquote in the AST
      case Select(Typed(tree, _), "unquote") => // TODO Further specialize by checking that the type is Quoted[T]?
        //println("=============== NEW Unquoted RHS ================\n" + AstPrinter().apply(tree))
        println("Case Unquote Selector")
        tree.seal match {
          case '{ Quoted[$t]($ast) } => unlift(ast) // doesn't work with 'new'
        }

      // When doing the 'unquote' method it adds an extra Typed node since that function has a Type
      case Typed(Select(Typed(tree, _), "unquote"), _) => // TODO Further specialize by checking that the type is Quoted[T]?
        //println("=============== NEW Unquoted RHS Typed ================\n" + AstPrinter().apply(tree))
        println("Case Unquote Typed Selector")
        tree.seal match {
          case '{ Quoted[$t]($ast) } => unlift(ast) // doesn't work with 'new'
        }

      case Apply(Select(Seal(left), "*"), Seal(right) :: Nil) =>
        println("Apply Select")
        BinaryOperation(astParser(left), NumericOperator.*, astParser(right))

      case Select(Seal(prefix), member) =>
        println("Select")
        Property(astParser(prefix), member)

      case Ident(x) => 
        println("Ident")
        Idnt(x)


      case t =>
        println("=============== Parsing Error ================\n" + AstPrinter().apply(t))
        println("=============== Extracted ================\n" + t.showExtractors)
        ???
        //println(t)
        //summon[QuoteContext].error("Parsing error: " + in.show, in)
        //???
    }
  }

}