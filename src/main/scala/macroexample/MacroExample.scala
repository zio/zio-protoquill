package macroexample

import scala.quoted._



trait Foo {
  def value: String
}
case class Bar(value: String) extends Foo
case class Baz(value: String) extends Foo

object MacroExample {

  // detectPlus(numberOne + numberInt)

  inline def getMethods(inline expr: Any): Any = ${ geteMethodsImpl('expr) }
  def geteMethodsImpl(exprRaw: Expr[Any])(using Quotes): Expr[Any] = {
    import quotes.reflect._
    import scala.collection.JavaConverters._
    // Note is a TypeRef and a TypeTree
    
    val expr = Term.of(exprRaw).underlyingArgument.asExpr
    printer.lnf(Term.of(expr).tpe.widen.classSymbol)
    val sym = Term.of(expr).tpe.widen.classSymbol // To Go from TermRef(p) to TypeRef(Person)
    
    val namesAndApplys =
      sym.get.caseFields.map(field => {
        (field.name, Select(Term.of(expr), field).asExprOf[Any])
      })

    val mapFields = namesAndApplys.map { case (name, applyField) =>
      '{ (${Expr(name)}, $applyField) }
    }
    '{ Map[String, Any](${Expr.ofList(mapFields)}:_*) }

    //val sym = Term.of(expr).tpe.asInstanceOf[TypeRef].symbol
    //sym.get.caseFields.foreach(println(_))
    //printer.lnf(field.tree)

    // println(tpe.widen) // TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)
    //println(tpe.decls.getClass.getMethods.filter(_.isAccessible).map(_.getName).foreach(println(_)))
    //println(tpe.classSymbol.get.asTerm)
    //'{ () }
  }

  inline def showTreeMatchLambda(inline expr: (String, String) => Int): Unit = ${ showTreeMatchLambdaImpl('expr) }
  def showTreeMatchLambdaImpl(expr: Expr[(String, String) => Int])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    printer.lnf(Term.of(expr).underlyingArgument)
    Term.of(expr).underlyingArgument match {
      case Lambda(List(ValDef(argName, _, _), ValDef(argName1, _, _)), body) =>
        println("Arg is: " + argName + " and " + argName1)
        println("Body is: " + body.showExtractors)
    }

    '{ () }
  }

  inline def showTree(inline expr: Any): Unit = ${ showTreeImpl('expr) }
  def showTreeImpl(expr: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    //println(Term.of(expr).underlyingArgument.showExtractors)
    printer.lnf(Term.of(expr).underlyingArgument)
    '{ () }
  }

  inline def detectPlus(inline expr: Int): (Int, String) = ${ detectPlusImpl('expr) }
  def detectPlusImpl(expr: Expr[Int])(using Quotes): Expr[(Int, String)] = {
    import quotes.reflect._
    println(Term.of(expr).underlyingArgument.showExtractors)

    val message = 
      Term.of(expr).underlyingArgument.asExpr match {
        case '{ ($one: Int).+(($two: Int)) } => 
          val oneInner = Term.of(one) match { case Ident(value) => value }
          val twoInner = Term.of(two) match { case Ident(value) => value }

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

  def macroTestImpl(somethingMakingStringBoolRaw: Expr[Boolean])(using Quotes): Expr[String] = {
    import quotes.reflect._
    val somethingMakingBool = Term.of(somethingMakingStringBoolRaw).underlyingArgument.asExpr

    val theExpressionAst = Term.of(somethingMakingBool).showExtractors
    val theExpressionCode = Term.of(somethingMakingBool).show
    '{
      if ($somethingMakingStringBoolRaw) 
        "Yay, we passed!... " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
      else 
        "We did not pass: " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
    }
  }
}


