
package miniquill.parser

import miniquill.ast._
import miniquill.quoter._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import scala.deriving._


class Lifter(given qctx:QuoteContext) extends PartialFunction[Ast, Expr[Ast]] {
  import qctx.tasty._

  def apply(ast: Ast): Expr[Ast] = liftAst(ast)
  def isDefinedAt(ast: Ast): Boolean = liftAst.isDefinedAt(ast)

  def liftAst: PartialFunction[Ast, Expr[Ast]] = {
    case Const(v) => '{ Const(${Expr(v)}) }
    case Entity(name: String) => '{ Entity(${Expr(name)})  }
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${liftAst(a)}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${liftAst(b)})  }
    case Property(ast: Ast, name: String) => '{Property(${liftAst(ast)}, ${Expr(name)}) }
    //case ScalarValueLift(name: String, value: Tree) => 
    //  '{ScalarValueLift(${Expr(name)}, value)}
  }

  def liftOperator: PartialFunction[Operator, Expr[Operator]] = {
    case NumericOperator.* => '{ NumericOperator.* }
  }
}

class Unlifter(given qctx:QuoteContext) extends PartialFunction[Expr[Ast], Ast] {

def apply(astExpr: Expr[Ast]): Ast = unliftAst(astExpr)
def isDefinedAt(astExpr: Expr[Ast]): Boolean = unliftAst.isDefinedAt(astExpr)

def unliftAst: PartialFunction[Expr[Ast], Ast] = {
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
    case '{ Map(${query}, ${alias}, ${body}: Ast) } => Map(unliftAst(query), unliftAst(alias).asInstanceOf[Idnt], unliftAst(body))
    case '{ BinaryOperation(${a}, ${operator}, ${b}: Ast) } => BinaryOperation(unliftAst(a), unliftOperator(operator).asInstanceOf[BinaryOperator], unliftAst(b))
    case '{ Property(${ast}, ${name}) } =>
      import scala.quoted.matching.{Const => Constant}
      val unname = name match {
        case Constant(v: String) => v
      }
      Property(unliftAst(ast), unname)

    case '{ScalarValueLift(${name}, ${value})} => 
      import scala.quoted.matching.{Const => Constant}
      val unname = name match {
        case Constant(v: String) => v
      }
      ScalarValueLift(unname, null)
  }

  def unliftOperator: PartialFunction[Expr[Operator], Operator] = {
    case '{ NumericOperator.* } =>  NumericOperator.*
  }
}

class Parser(given qctx:QuoteContext) extends PartialFunction[Expr[_], Ast] {
  import qctx.tasty.{Type => _, _, given}
  import printer.ContextAstPrinter._

  val unlift = new Unlifter()

  
    
    private object Seal {
      def unapply[T](e: Term) = {
        implicit val ttpe: Type[T] = e.tpe.seal.asInstanceOf[Type[T]]

        Some(e.seal.cast[T])
      }
    }

    private object Unseal {
      def unapply(t: Expr[Any]) = {
        Some(t.unseal)
      }
    }

    private object PossibleTypeApply {
      def unapply[T](tree: Tree): Option[Tree] =
        tree match {
          case TypeApply(inner, _) => Some(inner)
          case other => Some(other)
        }
    }

    // TODO LambdaN ?
    private object Lambda1 {
      def unapply(term: Term) = term match {
        case Lambda(ValDef(ident, _, _) :: Nil, Seal(methodBody)) => Some((ident, methodBody))
      }
    }

    private object TypedMatroshka {
      // need to define a case where it won't go into matcher otherwise recursion is infinite
      //@tailcall // should be tail recursive
      def recurse(innerTerm: Term): Term = innerTerm match {
        case Typed(innerTree, _) => recurse(innerTree)
        case other => other
      }

      def unapply(term: Term): Option[Term] = term match {
        case Typed(tree, _) => Some(recurse(tree))
        case _ => None
      }
    }


    def apply(in: Expr[_]): Ast = {
      println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WHOLE TREE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      println(AstPrinter.astprint(in.unseal.underlyingArgument).render.split("\n").map("%% " + _).mkString("\n"))
      
      // in.unseal.underlyingArgument.seal match {
      //   //case vv @ '{ (($tree:Quoted[$t]).unquote): $at } =>
  
      //   case vv @ Unseal(Typed( Seal('{ ($tree:Quoted[$t]).unquote } ), tpe)) =>
      //     println("***** NEW Case Unquote Selector: " + AstPrinter().apply(vv.unseal))
          
      //   case _ => println("********* no k match **********")
      // }

      astParser(in.unseal.underlyingArgument.seal)
    }

    def isDefinedAt(in: Expr[_]): Boolean =
      astParser.isDefinedAt(in.unseal.underlyingArgument.seal)

    def astParser: PartialFunction[Expr[_], Ast] = {
      // Skip through type ascriptions
      case Unseal(TypedMatroshka(tree)) =>
        println("Going into: " + printer.ln(tree))
        astParser(tree.seal)

      case Unseal(Inlined(_, _, v)) =>
        println("Case Inlined")
        //astParser(v.seal.cast[T]) // With method-apply can't rely on it always being T?
        astParser(v.seal)

        // TODO Need to figure how how to do with other datatypes
      case Unseal(Literal(Constant(v: Double))) => 
        println("Case Literal Constant")
        Const(v)
      
      case 
        Unseal(
          Apply(
            TypeApply(
              // Need to use showExtractors to get TypeIdentt
              Select(New(TypeIdent("EntityQuery")), /* <Init> */ _), List(targ)
            ), _
          )
        )
        =>
        println("Typed Apply of EntityQuery")
        val name: String = targ.tpe.classSymbol.get.name
        Entity(name)


      case vv @ '{ ($q:Query[$qt]).map[$mt](${Unseal(Lambda1(ident, body))}) } => 
        Map(astParser(q), Idnt(ident), astParser(body))

      // When just doing .unquote in the AST
      case Unseal(vv @ Select(TypedMatroshka(tree), "unquote")) => // TODO Further specialize by checking that the type is Quoted[T]?
        //println("=============== NEW Unquoted RHS ================\n" + AstPrinter().apply(tree))
        println("Case Unquote Selector: " + AstPrinter().apply(vv))
        tree.seal match {
          // TODO Need an unlift error if there's no match
          case '{ Quoted[$t]($ast) } => unlift(ast) // doesn't work with 'new'
        }

      // When doing the 'unquote' method it adds an extra Typed node since that function has a Type
      case Unseal(vv @ TypedMatroshka(Select(TypedMatroshka(tree), "unquote"), _)) => // TODO Further specialize by checking that the type is Quoted[T]?
        //println("=============== NEW Unquoted RHS Typed ================\n" + AstPrinter().apply(tree))
        println("Case Unquote Typed Selector" + AstPrinter().apply(tree))
        tree.seal match {
          // TODO Need an unlift error if there's no match
          case '{ Quoted[$t]($ast) } => unlift(ast) // doesn't work with 'new'
        }

      case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
        println("Apply Select")
        BinaryOperation(astParser(left), NumericOperator.*, astParser(right))

      case Unseal(Select(Seal(prefix), member)) =>
        println("Select")
        Property(astParser(prefix), member)

      case Unseal(Ident(x)) => 
        println("Ident")
        Idnt(x)

      // case Unseal(t) =>
      //   println("=============== Parsing Error ================\n" + AstPrinter().apply(t))
      //   println("=============== Extracted ================\n" + t.showExtractors)
      //   ???
      //   //println(t)
      //   //summon[QuoteContext].error("Parsing error: " + in.show, in)
      //   //???
    }
}