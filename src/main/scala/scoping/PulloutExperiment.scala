package scoping

import scala.quoted._

import scala.deriving.ArrayProduct
//import dotty.tools.dotc.ast.untpd._

case class LookInside[T](value: T, id: String)

object PulloutExperiment {

  inline def printTree[T](value: T):T = ${ printTreeImpl('value) }
  def printTreeImpl[T: Type](value: Expr[T])(using Quotes): Expr[T] = {
    import quotes.reflect._
    //printer.ln(Term.of(value)e).underlyingArgument)
    printer.ln("===================== printTree ================\n")
    printer.ln(Term.of(value).underlyingArgument)
    println(Printer.TreeStructure.show(Term.of(value).underlyingArgument))
    value
  }
  
  
  inline def lookInside[T](value: T): LookInside[T] = ${lookInsideImpl('value)}
  def lookInsideImpl[T: Type](value: Expr[T])(using Quotes): Expr[LookInside[T]] = {
    val uuid = java.util.UUID.randomUUID().toString
    '{ LookInside[T]($value, ${Expr(uuid)}) }
  }

  // TODO nest step would be to extract all uuids and get a particular one?

  // Then next step afterward would be to have a function that adds a value to a tuple

  inline def parseTuple(input: Tuple): List[LookInside[_]] = ${parseTupleImpl('input)}
  def parseTupleImpl(input: Expr[Tuple])(using Quotes): Expr[List[LookInside[_]]] = {
    import quotes.reflect._
    import scala.collection.mutable.ArrayBuffer

    // Can also expore using TreeAccumulator to find LookInside instances
    // this might be easier however
  
    '{ 
      $input.asInstanceOf[Product]
      .productIterator.map(_.asInstanceOf[LookInside[_]])
      .toList 
    }
  }

  trait Expresser[T]:
    def express(t: T): String

  given Expresser[String] = new Expresser:
    def express(t: String) = s"String--(${t})"

  given Expresser[Int] = new Expresser:
    def express(t: Int) = s"Int--(${t})"



  inline def summonExprUsers(input: Tuple): List[(String, String)] = ${summonExprUsersImpl('input)}
  def summonExprUsersImpl(input: Expr[Tuple])(using Quotes): Expr[List[(String, String)]] = {
    import quotes.reflect._
    import scala.collection.mutable.ArrayBuffer

    println("===================== Summon Expressers Value =====================")
    println(Term.of(input).underlyingArgument.show)
  
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(owner: Symbol) = tree match {
        case arg @ Apply(TypeApply(Select(Ident("LookInside"), "apply"), _), List(transportValue, Literal(StringConstant(uid)))) => 
          printer.ln("Found: " + arg)
          terms += arg
        case _ => 
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)(owner)
      }
    }

    

    val lifts = accum.foldTree(ArrayBuffer.empty, Term.of(input).underlyingArgument)(Symbol.spliceOwner).map(_.asExpr)
    
    val identifiedLifts =
      lifts.map {
        case term @ '{ LookInside[tpe]($qvalue, $quotedUid) } => // find unique uids?
          val uid = quotedUid match {
            case Const(v: String) => v
          }
          (uid, term)
      }.toMap

    val encodedLifts =
      identifiedLifts.toList.map { case (k, v) => // typing is wrong when you exclude 'case'
        val encoded = 
          v match {
            case '{ LookInside[tpe]($value, $uid) } => // find unique uids?
              Expr.summon[Expresser[tpe]] match {
                case Some(expresserExpr) => '{ $expresserExpr.express($value) }
                case None => throw new RuntimeException(s"Could not find expresser for ${Type.show[Expresser[tpe]]}")
              }
            case other => throw new RuntimeException(s"The term ${Term.of(other).underlyingArgument.show} is not a LookInside")
          }
        '{ (${Expr(k)}, $encoded) }
      }

    encodedLifts.foldRight('{ (Nil: List[(String, String)]) })((elem, term) => '{ $elem :: $term })
  }


  inline def addElementToTuple[T](tup: Tuple, elem: T): Tuple = ${addElementToTupleImpl('tup, 'elem)}
  def addElementToTupleImpl[T: Type](tup: Expr[Tuple], elem: Expr[T])(using Quotes): Expr[Tuple] = {
    import quotes.reflect._
    '{ (${elem} *: ${tup}) }
  }

  inline def matchList(list: List[Any]): List[Any] = ${ matchListImpl('list) }
  def matchListImpl(list: Expr[List[Any]])(using Quotes): Expr[List[Any]] = {
    import quotes.reflect._
    println(Term.of(list).underlyingArgument.asExpr.show)
    val elems = 
      list match {
        case '{ List[t](${Varargs(elems)}: _*) } => elems.toList
        case _ => List()
      }
    println(s"Found Elems: ${elems.map(Term.of(_).show)}")
    Expr.ofList(elems)
  }

  inline def pullout(input: Any): Tuple = ${pulloutImpl('input)}
  def pulloutImpl(input: Expr[Any])(using Quotes): Expr[Tuple] = {
    import quotes.reflect._
    //import quotes.reflect.using_IsInstanceOf_Term
    import scala.collection.mutable.ArrayBuffer

    println(Term.of(input).underlyingArgument.show)
    
    val accum = new TreeAccumulator[ArrayBuffer[Term]] {
      def foldTree(terms: ArrayBuffer[Term], tree: Tree)(owner: Symbol) = tree match {
        case arg @ Apply(TypeApply(Select(Ident("LookInside"), "apply"), _), List(transportValue, Literal(StringConstant(uid)))) => 
          printer.ln("Found: " + arg)
          terms += arg
        case _ => 
          printer.ln("***** NOT FOUND ****")
          foldOverTree(terms, tree)(owner)
      }
    }

    //printer.ln(Term.of(input).underlyingArgument)
    //printer.ln(Term.of(input).underlyingArgument.showExtractors)

    val instances = accum.foldTree(ArrayBuffer.empty, Term.of(input).underlyingArgument)(Symbol.spliceOwner)

    instances.zipWithIndex.map { case (v, i) => printer.ln(s"Element: ($i) $v") }

    val ret =
     instances.foldRight('{ EmptyTuple: Tuple })((elem, term) => '{ ( ${elem.asExpr} *: ${term} ) })

    printer.ln("=========== Pullout Value =========\n" + Term.of(ret).underlyingArgument.show)

    ret
  }
}