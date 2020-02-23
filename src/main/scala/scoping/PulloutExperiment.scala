package scoping

import scala.quoted._
import scala.quoted.matching._
import scala.deriving.ArrayProduct
//import dotty.tools.dotc.ast.untpd._

case class LookInside[T](value: T, id: String)

object PulloutExperiment {

  inline def printTree[T](value: T):T = ${ printTreeImpl('value) }
  def printTreeImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{given _, _}
    //printer.ln(value.unseal.underlyingArgument)
    printer.ln("===================== printTree ================\n")
    printer.ln(value.unseal.underlyingArgument)
    println(value.unseal.underlyingArgument.showExtractors)
    value
  }
  
  
  inline def lookInside[T](value: T): LookInside[T] = ${lookInsideImpl('value)}
  def lookInsideImpl[T: Type](value: Expr[T])(given qctx: QuoteContext): Expr[LookInside[T]] = {
    val uuid = java.util.UUID.randomUUID().toString
    '{ LookInside[T]($value, ${Expr(uuid)}) }
  }

  // TODO nest step would be to extract all uuids and get a particular one?

  // Then next step afterward would be to have a function that adds a value to a tuple

  inline def parseTuple(input: Tuple): List[LookInside[_]] = ${parseTupleImpl('input)}
  def parseTupleImpl(input: Expr[Tuple])(given qctx: QuoteContext): Expr[List[LookInside[_]]] = {
    import qctx.tasty.{given _, _}
    import scala.collection.mutable.ArrayBuffer

    // Can also expore using TreeAccumulator to find LookInside instances
    // this might be easier however
  
    '{ 
      $input.asInstanceOf[Product]
      .productIterator.map(_.asInstanceOf[LookInside[_]])
      .toList 
    }
  }

  trait Expresser[T] with
    def express(t: T): String

  given Expresser[String] = new Expresser with
    def express(t: String) = s"String--(${t})"

  given Expresser[Int] = new Expresser with
    def express(t: Int) = s"Int--(${t})"



  inline def summonExpressers(input: Tuple): List[(String, String)] = ${summonExpressersImpl('input)}
  def summonExpressersImpl(input: Expr[Tuple])(given qctx: QuoteContext): Expr[List[(String, String)]] = {
    import qctx.tasty.{given _, _}
    import scala.collection.mutable.ArrayBuffer

    println("===================== Summon Expressers Value =====================")
    println(input.unseal.underlyingArgument.show)
  
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(implicit ctx: Context) = tree match {
        case arg @ Apply(TypeApply(Select(Ident("LookInside"), "apply"), _), List(transportValue, Literal(Constant(uid)))) => 
          printer.ln("Found: " + arg)
          terms += arg
        case _ => 
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)
      }
    }

    

    val lifts = accum.foldTree(ArrayBuffer.empty, input.unseal.underlyingArgument).map(_.seal)
    
    val identifiedLifts =
      lifts.map {
        case term @ '{ LookInside[$tpe]($qvalue, $quotedUid) } => // find unique uids?
          val uid = quotedUid match {
            case Const(v: String) => v
          }
          (uid, term)
      }.toMap

    val encodedLifts =
      identifiedLifts.toList.map { case (k, v) => // typing is wrong when you exclude 'case'
        val encoded = 
          v match {
            case '{ LookInside[$tpe]($value, $uid) } => // find unique uids?
              val expressType =  '[Expresser[$tpe]]
              summonExpr(given expressType) match {
                case Some(expresserExpr) => '{ $expresserExpr.express($value) }
                case None => throw new RuntimeException(s"Could not find expresser for ${expressType.unseal.show}")
              }
            case other => throw new RuntimeException(s"The term ${other.unseal.underlyingArgument.show} is not a LookInside")
          }
        '{ (${Expr(k)}, $encoded) }
      }

    encodedLifts.foldRight('{ (Nil: List[(String, String)]) })((elem, term) => '{ $elem :: $term })
  }


  inline def addElementToTuple[T](tup: Tuple, elem: T): Tuple = ${addElementToTupleImpl('tup, 'elem)}
  def addElementToTupleImpl[T: Type](tup: Expr[Tuple], elem: Expr[T])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{given _, _}
    '{ (${elem} *: ${tup}) }
  }

  inline def matchList(list: List[Any]): List[Any] = ${ matchListImpl('list) }
  def matchListImpl(list: Expr[List[Any]])(given qctx: QuoteContext): Expr[List[Any]] = {
    import qctx.tasty.{given, _}
    println(list.unseal.underlyingArgument.seal.show)
    val elems = 
      list match {
        case '{ List[$t](${ExprSeq(elems)}: _*) } => elems.toList
        case _ => List()
      }
    println(s"Found Elems: ${elems.map(_.unseal.show)}")
    '{ ${Expr.ofList(elems)} }
  }

  inline def pullout(input: Any): Tuple = ${pulloutImpl('input)}
  def pulloutImpl(input: Expr[Any])(given qctx: QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{_, given _}
    import qctx.tasty.given
    //import qctx.tasty.given_IsInstanceOf_Term
    import scala.collection.mutable.ArrayBuffer

    println(input.unseal.underlyingArgument.show)

    
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(implicit ctx: Context) = tree match {
        case arg @ Apply(TypeApply(Select(Ident("LookInside"), "apply"), _), List(transportValue, Literal(Constant(uid)))) => 
          printer.ln("Found: " + arg)
          terms += arg
        case _ => 
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)
      }
    }

    //printer.ln(input.unseal.underlyingArgument)
    //printer.ln(input.unseal.underlyingArgument.showExtractors)

    val instances = accum.foldTree(ArrayBuffer.empty, input.unseal.underlyingArgument)

    instances.zipWithIndex.map { case (v, i) => printer.ln(s"Element: ($i) $v") }

    val ret =
     instances.foldRight('{ (): Tuple })((elem, term) => '{ ( ${elem.seal} *: ${term} ) })

    printer.ln("=========== Pullout Value =========\n" + ret.unseal.underlyingArgument.show)

    ret
  }
}