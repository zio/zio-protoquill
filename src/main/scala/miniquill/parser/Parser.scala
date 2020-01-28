
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
        //println("********************** MATCHED VASE INNER TREE **********************")
        //printer.lnf(expr.unseal)
        Some(ast)
      case Unseal(TypedMatroshka(tree)) => unapply(tree.seal)
      case _ => 
        //println("********************** NOT MATCHED VASE INNER TREE **********************")
        //printer.lnf(expr.unseal)
        None
    }
  }

  protected object MatchVaseApply {
    def unapply(expr: Expr[Any]) = expr match {
      case vase @ '{ QuotationVase.apply[$qt]($quotation, ${scala.quoted.matching.Const(uid: String)}) } => 
        //println("********************** MATCHED VASE APPLY **********************")
        //printer.lnf(expr.unseal)
        Some((quotation, uid, vase))
      case _ => None
    }
  }

  // Match the QuotationVase(...).unquote values which are tacked on to every
  // child-quote (inside of a parent quote) when the 'unquote' function (i.e macro)
  // is applied.
  protected object MatchQuotationUnquote {
    def unapply(expr: Expr[Any]) = expr match {
      // When a QuotationVase is embedded into an ast
      case '{ (${quotationVase}: QuotationVase[$tt]).unquote } => Some(quotationVase)
      case _ => None
    }
  }

  object MatchRuntimeQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Any], String)] =
      expr match {
        // case MatchQuotationRef(tree, uuid) => 
        //   println("******************** Runtime: Match Quotation Ref ********************")
        //   printer.lnf((tree.unseal, uuid))
        //   Some((tree, uuid))
        case MatchQuotationUnquote(innards) =>
          println("******************** Runtime: Match Unquote ********************")
          printer.lnf(innards.unseal)
          unapply(innards)
        // sometimes there are multiple levels of vases when references are spliced,
        // we should only care about the innermost one
        case MatchVaseApply(_, uuid, vase) =>
          println("******************** Runtime: Vase Apply ********************")
          printer.lnf(uuid, vase)
          Some((vase, uuid))
        case _ => None
      }
    }

  object MatchInlineQuotation {
    def unapply(expr: Expr[Any]): Option[(Expr[Ast], String)] =
      expr match {
        case MatchQuotationUnquote(MatchVaseApply(MatchQuotedInnerTree(astTree), uuid, _)) =>
          Some((astTree, uuid))
        case _ => None
      }
  }
}

class Parser(given qctx:QuoteContext) extends PartialFunction[Expr[_], Ast] {
  import qctx.tasty.{Type => TType, _, given}
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
    printer.ln("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% WHOLE TREE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    printer.ln(in.unseal.underlyingArgument)
    
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

    case Unseal(value @ Select(Seal(prefix), member)) =>
      println(s"Select ${value.show}")
      if (value.tpe <:< '[io.getquill.Embedded].unseal.tpe) {
        println("Visibility: Hidden")
        Property.Opinionated(astParser(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        println("Visibility: Visible")
        Property(astParser(prefix), member)
      }

    case Unseal(Ident(x)) => 
      println("Ident")
      Idnt(x)



  // TODO define this a last-resort printing function inside the parser
  case Unseal(t) =>
    println("=============== Parsing Error ================\n" + AstPrinter().apply(t))
    println("=============== Extracted ================\n" + t.showExtractors)
    ???
    //println(t)
    //summon[QuoteContext].error("Parsing error: " + in.show, in)
    //???
  }
}