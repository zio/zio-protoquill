
package miniquill.parser

import io.getquill.ast.{Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._
import scala.quoted.matching._
import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable

// TODO Pluggable-in unlifter via implicit? Quotation dsl should have it in the root?
class Parser(given qctx:QuoteContext) extends PartialFunction[Expr[_], Ast] {
  import qctx.tasty.{Type => TType, _, given}

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

  def apply(inRaw: Expr[_]): Ast = {
    // NOTE Can disable if needed and make in = inRaw. See https://github.com/lampepfl/dotty/pull/8041 for detail
    val in = inRaw.unseal.underlyingArgument.seal
    astParser(in)
  }

  def isDefinedAt(in: Expr[_]): Boolean =
    astParser.isDefinedAt(in.unseal.underlyingArgument.seal)

  def astParser: PartialFunction[Expr[_], Ast] = {

    // Needs to be somewhere in the beginning so 'value' will not be parsed as a functiona-apply
    // i.e. since we don't want Property(..., value) to be the tree in this case.
    //case '{ (ScalarValueVase.apply[$t]($value, ${scala.quoted.matching.Const(uid: String)})).value } =>
    //  ScalarTag(uid)

    //case Unseal(Apply(TypeApply(Select(Ident("ScalarValueVase"), "apply"), List(Inferred())), List(scalaTree, Literal(Constant(uid: String))))) =>
    //  ScalarTag(uid)

    case MatchInlineQuotation(astTree, uid) =>
      unlift(astTree)

    case MatchLift(tree, uid) =>
      ScalarTag(uid)

    case MatchEncodeableLift(tree, uid) =>
      ScalarTag(uid) // TODO Want special scalar tag for an encodeable scalar

    // MUST come after the MatchInlineQuotation because it matches
    // the same kind of statement
    case MatchRuntimeQuotation(tree, uid) =>
      QuotationTag(uid)

    case `Quoted.apply`(ast) =>
      unlift(ast)

    case Unseal(Inlined(_, _, v)) =>
      //println("Case Inlined")
      //astParser(v.seal.cast[T]) // With method-apply can't rely on it always being T?
      astParser(v.seal)

      // TODO Need to figure how how to do with other datatypes
    case Unseal(Literal(Constant(v: Double))) => 
      //println("Case Literal Constant")
      Const(v)

    case Unseal(Literal(Constant(v: String))) => 
      //println("Case Literal Constant")
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
      val name: String = targ.tpe.classSymbol.get.name
      Entity(name, List())


    case vv @ '{ ($q:Query[$qt]).map[$mt](${Unseal(Lambda1(ident, body))}) } => 
      Map(astParser(q), Idnt(ident), astParser(body))
      
  

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

    // TODO Need to check if entity is a string
    case Unseal(Apply(Select(Seal(left), "+"), Seal(right) :: Nil)) =>
      BinaryOperation(astParser(left), StringOperator.+, astParser(right))

    case Unseal(Apply(Select(Seal(left), "*"), Seal(right) :: Nil)) =>
      BinaryOperation(astParser(left), NumericOperator.*, astParser(right))

    case Unseal(value @ Select(Seal(prefix), member)) =>
      //println(s"Select ${value.show}")
      //val sealedTpe = value.tpe.seal
      if ((value.tpe <:< '[io.getquill.Embedded].unseal.tpe)) { 
        // TODO Figure how how to check the summon here
        // || (summonExpr(given '[Embedable[$tpee]]).isDefined)
        Property.Opinionated(astParser(prefix), member, Renameable.ByStrategy, Visibility.Hidden)
      } else {
        Property(astParser(prefix), member)
      }

    case Unseal(Ident(x)) => 
      //println("Ident")
      Idnt(x)

    // If at the end there's an inner tree that's typed, move inside and try to parse again
    case Unseal(Typed(innerTree, _)) =>
      astParser(innerTree.seal)

  // TODO define this a last-resort printing function inside the parser
  case Unseal(t) =>
    println("=============== Parsing Error ================\n" + printer.ln(t))
    println("=============== Extracted ================\n" + t.showExtractors)
    ???
    //println(t)
    //summon[QuoteContext].error("Parsing error: " + in.show, in)
    //???
  }
}