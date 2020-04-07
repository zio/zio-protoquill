package macroexample

import scala.quoted._
import scala.quoted.matching._


trait Foo {
  def value: String
}
case class Bar(value: String) extends Foo
case class Baz(value: String) extends Foo

object MacroExample {

  // detectPlus(numberOne + numberInt)

  inline def getMethods(inline expr: Any): Any = ${ geteMethodsImpl('expr) }
  def geteMethodsImpl(exprRaw: Expr[Any])(given qctx: QuoteContext): Expr[Any] = {
    import qctx.tasty.{given, _}
    import scala.collection.JavaConverters._
    // Note is a TypeRef and a TypeTree
    
    val expr = exprRaw.unseal.underlyingArgument.seal
    printer.lnf(expr.unseal.tpe.widen.classSymbol)
    val sym = expr.unseal.tpe.widen.classSymbol // To Go from TermRef(p) to TypeRef(Person)
    
    val namesAndApplys =
      sym.get.caseFields.map(field => {
        (field.name, Select(expr.unseal, field).seal.cast[Any])
      })

    val mapFields = namesAndApplys.map { case (name, applyField) =>
      '{ (${Expr(name)}, $applyField) }
    }
    '{ Map[String, Any](${Expr.ofList(mapFields)}:_*) }

    //val sym = expr.unseal.tpe.asInstanceOf[TypeRef].symbol
    //sym.get.caseFields.foreach(println(_))
    //printer.lnf(field.tree)

    // println(tpe.widen) // TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)
    //println(tpe.decls.getClass.getMethods.filter(_.isAccessible).map(_.getName).foreach(println(_)))
    //println(tpe.classSymbol.get.asTerm)
    //'{ () }
  }

  inline def showTreeMatchLambda(inline expr: (String, String) => Int): Unit = ${ showTreeMatchLambdaImpl('expr) }
  def showTreeMatchLambdaImpl(expr: Expr[(String, String) => Int])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{given, _}
    printer.lnf(expr.unseal.underlyingArgument)
    expr.unseal.underlyingArgument match {
      case Lambda(List(ValDef(argName, _, _), ValDef(argName1, _, _)), body) =>
        println("Arg is: " + argName + " and " + argName1)
        println("Body is: " + body.showExtractors)
    }

    '{ () }
  }

  inline def showTree(inline expr: Any): Unit = ${ showTreeImpl('expr) }
  def showTreeImpl(expr: Expr[Any])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{given, _}
    //println(expr.unseal.underlyingArgument.showExtractors)
    printer.lnf(expr.unseal.underlyingArgument)
    '{ () }
  }

  inline def detectPlus(inline expr: Int): (Int, String) = ${ detectPlusImpl('expr) }
  def detectPlusImpl(expr: Expr[Int])(given qctx: QuoteContext): Expr[(Int, String)] = {
    import qctx.tasty.{given, _}
    println(expr.unseal.underlyingArgument.showExtractors)

    val message = 
      expr.unseal.underlyingArgument.seal match {
        case '{ ($one: Int).+(($two: Int)) } => 
          val oneInner = one.unseal match { case Ident(value) => value }
          val twoInner = two.unseal match { case Ident(value) => value }

          s"(${oneInner}, ${twoInner}, +)"
       
        case _ => "addition of two things not detected"
      }
    
    '{ ($expr, ${Expr(message)}) }
  }


  def normalTest(expr: Boolean): String = {
    if (expr) "Yay, we passed!... something"
    else "We did not pass"
  }

  inline def macroTest(inline somethingMakingStringBoolRaw: Boolean): String = 
    ${ macroTestImpl('somethingMakingStringBoolRaw) }

  def macroTestImpl(somethingMakingStringBoolRaw: Expr[Boolean])(given qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{given, _}
    val somethingMakingBool = somethingMakingStringBoolRaw.unseal.underlyingArgument.seal

    val theExpressionAst = somethingMakingBool.unseal.showExtractors
    val theExpressionCode = somethingMakingBool.unseal.show
    '{
      if ($somethingMakingStringBoolRaw) 
        "Yay, we passed!... " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
      else 
        "We did not pass: " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
    }
  }
}


