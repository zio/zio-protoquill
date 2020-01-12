
package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.annotation.StaticAnnotation
import printer.AstPrinter
import scala.deriving._


class QuotationParser(given qctx: QuoteContext) {
  import qctx.tasty.{_, given}
  
  // TODO Refactor out into a trait?
  private object Unseal {
    def unapply(t: Expr[Any]) = {
      Some(t.unseal)
    }
  }

  import qctx.tasty._
  import scala.quoted.matching.{Const => Constant} //hello

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

  protected object MatchQuotedInnerTree {
    def unapply(expr: Expr[Any]): Option[Expr[Ast]] = expr match {
      case '{ Quoted.apply[$qt]($ast, $v) } => 
        println("********************** MATCHED VASE INNER TREE **********************")
        printer.ln(expr.unseal)
        Some(ast)
      case Unseal(TypedMatroshka(tree)) => unapply(tree.seal)
      case _ => 
        println("********************** NOT MATCHED VASE INNER TREE **********************")
        printer.ln(expr.unseal)
        None
    }
  }

  protected object MatchVaseApply {
    def unapply(expr: Expr[Any]) = expr match {
      case '{ QuotationVase.apply[$qt]($quotation, ${scala.quoted.matching.Const(uid: String)}) } => 
        println("********************** MATCHED VASE APPLY **********************")
        Some((quotation, uid))
      case _ => None
    }
  }

  // Match the QuotationVase(...).unquote values which are tacked on to every
  // child-quote (inside of a parent quote) when the 'unquote' function (i.e macro)
  // is applied.
  protected object MatchQuotationVase {
    def unapply(expr: Expr[Any]) = expr match {
      case '{ (${quotationVase}: QuotationVase[$tt]).unquote } => Some(quotationVase)
      case _ => None
    }
  }

  // protected object UnsealedIdent {
  //   def unapply(expr: Expr[Any]) = expr match {
  //     case v @ Unseal(Ident(innerQuote)) => Some(v)
  //     case _ => None
  //   }
  // }

  // Match an ident representing a quotation. It it matches, return the vase so we can use it later.
  protected object MatchQuotationRef {
    def unapply(expr: Expr[Any]): Option[(Expr[Any], String)] = expr match {

      // <TODO ASK EPFL> For some reason it's not possible to do '{ QuotationVase.apply[$t](t @ ${UnsealedIdent(quoteTerm)}, ${Constant(uid: String)}) }
      // it gives an error regarding the $t. Therefore I extracted this term into it's own matcher
      case vase @ '{ QuotationVase.apply[$t](${Unseal(Ident(innerQuote))}, ${Constant(uid: String)}) } =>
        Some((vase, uid))
      case _ => None
    }
  }

  object MatchRuntimeQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Any], String)] =
      expr match {
        case MatchQuotationVase(MatchQuotationRef(tree, uuid)) =>
          Some((tree, uuid))
        case _ => None
      }
    }

  object MatchInlineQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Ast], String)] =
      expr match {
        case MatchQuotationVase(MatchVaseApply(MatchQuotedInnerTree(astTree), uuid)) =>
          Some((astTree, uuid))
        case _ => None
      }
  }
}


class Lifter(given qctx:QuoteContext) extends PartialFunction[Ast, Expr[Ast]] {
  import qctx.tasty._

  def apply(ast: Ast): Expr[Ast] = liftAst(ast)
  def isDefinedAt(ast: Ast): Boolean = liftAst.isDefinedAt(ast)

  def liftAst: PartialFunction[Ast, Expr[Ast]] = {
    // TODO Need some type info to be able to lift a const
    //case Const(v) => '{ Const(${Expr(v)}) }
    case Entity(name: String, list) => '{ Entity(${Expr(name)}, List())  }
    case Idnt(name: String) => '{ Idnt(${Expr(name)})  }
    case Map(query: Ast, alias: Idnt, body: Ast) => '{ Map(${liftAst(query)}, ${liftAst(alias).asInstanceOf[Expr[Idnt]]}, ${liftAst(body)})  }
    case BinaryOperation(a: Ast, operator: BinaryOperator, b: Ast) => '{ BinaryOperation(${liftAst(a)}, ${liftOperator(operator).asInstanceOf[Expr[BinaryOperator]]}, ${liftAst(b)})  }
    case Property(ast: Ast, name: String) => '{Property(${liftAst(ast)}, ${Expr(name)}) }
    case ScalarValueTag(uid: String) => '{ScalarValueTag(${Expr(uid)})}
    case QuotationTag(uid: String) => '{QuotationTag(${Expr(uid)})}
  }

  def liftOperator: PartialFunction[Operator, Expr[Operator]] = {
    case NumericOperator.* => '{ NumericOperator.* }
  }
}

class Unlifter(given qctx:QuoteContext) extends PartialFunction[Expr[Ast], Ast] {

def apply(astExpr: Expr[Ast]): Ast = unliftAst(astExpr)
def isDefinedAt(astExpr: Expr[Ast]): Boolean = unliftAst.isDefinedAt(astExpr)

def unliftAst: PartialFunction[Expr[Ast], Ast] = {
    // TODO have a typeclass like Splicer to translate constant to strings
    case '{ Const(${b}) } =>
      import scala.quoted.matching.{Const => Constant}
      b match {
        case Constant(v: Double) => Const(v)
      }
    case '{ Entity(${b}, ${l})  } =>
      import scala.quoted.matching.{Const => Constant}
      b match {
        case Constant(v: String) => Entity(v, List()) // TODO Need to implement unlift of list
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

    case '{ScalarValueTag(${uid})} => 
      import scala.quoted.matching.{Const => Constant}
      val unuid = uid match {
        case Constant(v: String) => v
      }
      ScalarValueTag(unuid)

    case '{ QuotationTag($uid) } =>
      import scala.quoted.matching.{Const => Constant}
      val unuid = uid match {
        case Constant(v: String) => v
      }
      QuotationTag(unuid)
  }

  def unliftOperator: PartialFunction[Expr[Operator], Operator] = {
    case '{ NumericOperator.* } =>  NumericOperator.*
  }
}

class Parser(given qctx:QuoteContext) extends PartialFunction[Expr[_], Ast] {
  import qctx.tasty.{Type => _, _, given}
  import printer.ContextAstPrinter._

  val unlift = new Unlifter()
  val quotationParser = new QuotationParser
  import quotationParser._
  
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
    // Needs to be first: Skip through type ascriptions
    case Unseal(TypedMatroshka(tree)) =>
      println("Going into: " + printer.ln(tree))
      astParser(tree.seal)

    // Needs to be somewhere in the beginning so 'value' will not be parsed as a functiona-apply
    // i.e. since we don't want Property(..., value) to be the tree in this case.
    case '{ (ScalarValueVase.apply[$t]($value, ${scala.quoted.matching.Const(uid: String)})).value } =>
      ScalarValueTag(uid)

    //case Unseal(Apply(TypeApply(Select(Ident("ScalarValueVase"), "apply"), List(Inferred())), List(scalaTree, Literal(Constant(uid: String))))) =>
    //  ScalarValueTag(uid)

    case MatchRuntimeQuotation(tree, uid) =>
      QuotationTag(uid)

    case MatchInlineQuotation(astTree, uid) =>
      unlift(astTree)

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
      Entity(name, List())


    case vv @ '{ ($q:Query[$qt]).map[$mt](${Unseal(Lambda1(ident, body))}) } => 
      Map(astParser(q), Idnt(ident), astParser(body))



    // case vv @ '{ (${quotationVase}: QuotationVase[$tt]).unquote } =>
    //   import scala.quoted.matching.{Const => Constant}

    //   // TODO Need to return to top-level if nothing matched. Look at original quill
    //   // parsing for how to do that
    //   quotationVase match {
    //     // In this case, the quotation is a runtime value
    //     case '{ QuotationVase.apply[$t](${Unseal(Ident(innerQuote))}, ${Constant(uid: String)}) } =>
    //       QuotationTag(uid)
 
    //     // <TODO ASK EPFL> why this doesn't work and how to do it?
    //     //case '{ QuotationVase.apply[$qt](Quoted[$qt]($ast, $v), $uidConst) } =>
    //     case '{ QuotationVase.apply[$qt]($quotation, $uidConst) } => 



    //       quotation match {
    //         case '{ Quoted[$qt].apply($ast, $v) } => unlift(ast)
    //         case other => 
    //           println("))))))))))))))))))) Match not found (((((((((((((((")
    //           printer.ln(other.unseal)
    //           throw new RuntimeException("Cannot match expression")
    //       }
    //   }
      
      

    // // If the inner quoted is a runtime value
    // case Unseal(vv @ Select(Ident(tree), "unquote")) =>

    // // When just doing .unquote in the AST
    // case Unseal(vv @ Select(TypedMatroshka(tree), "unquote")) => // TODO Further specialize by checking that the type is Quoted[T]?
    //   //println("=============== NEW Unquoted RHS ================\n" + AstPrinter().apply(tree))
    //   println("===================== Case Unquote Selector: " + AstPrinter().apply(vv) + " =====================")
    //   tree.seal match {
    //     // TODO Need an unlift error if there's no match
    //     // Note, don't care about lifts here, they are extracted in Quotation

    //     // Try to file the case where _ is a bug here
    //     case '{ Quoted[$t]($ast, $v) } => unlift(ast) // doesn't work with 'new' // Note, replacing $v with _ causes compiler to compile forever
    //   }

    // // When doing the 'unquote' method it adds an extra Typed node since that function has a Type
    // // (I'm not sure that both this method as well as the one before it are needed)
    // case Unseal(vv @ TypedMatroshka(Select(TypedMatroshka(tree), "unquote"), _)) => // TODO Further specialize by checking that the type is Quoted[T]?
    //   //println("=============== NEW Unquoted RHS Typed ================\n" + AstPrinter().apply(tree))
    //   println("===================== Case Unquote Typed Selector" + AstPrinter().apply(tree) + "=====================")
    //   tree.seal match {
    //     // TODO Need an unlift error if there's no match
    //     // Note, don't care about lifts here, they are extracted in Quotation
    //     case '{ Quoted[$t]($ast, $v) } => unlift(ast) // doesn't work with 'new' // Note, replacing $v with _ causes compiler to compile forever
    //   }

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      println("Apply Select")
      BinaryOperation(astParser(left), NumericOperator.*, astParser(right))

    case Unseal(Select(Seal(prefix), member)) =>
      println("Select")
      Property(astParser(prefix), member)

    case Unseal(Ident(x)) => 
      println("Ident")
      Idnt(x)



  case Unseal(t) =>
    println("=============== Parsing Error ================\n" + AstPrinter().apply(t))
    println("=============== Extracted ================\n" + t.showExtractors)
    ???
    //println(t)
    //summon[QuoteContext].error("Parsing error: " + in.show, in)
    //???
  }
}