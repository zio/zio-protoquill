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
    
    val expr = exprRaw.asTerm.underlyingArgument.asExpr
    printer.lnf(expr.asTerm.tpe.widen.classSymbol)
    val sym = expr.asTerm.tpe.widen.classSymbol // To Go from TermRef(p) to TypeRef(Person)
    
    val namesAndApplys =
      sym.get.caseFields.map(field => {
        (field.name, Select(expr.asTerm, field).asExprOf[Any])
      })

    val mapFields = namesAndApplys.map { case (name, applyField) =>
      '{ (${Expr(name)}, $applyField) }
    }
    '{ Map[String, Any](${Expr.ofList(mapFields)}:_*) }

    //val sym = expr.asTerm.tpe.asInstanceOf[TypeRef].symbol
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
    printer.lnf(expr.asTerm.underlyingArgument)
    expr.asTerm.underlyingArgument match {
      case Lambda(List(ValDef(argName, _, _), ValDef(argName1, _, _)), body) =>
        println("Arg is: " + argName + " and " + argName1)
        println("Body is: " + Printer.TreeStructure.show(body))
    }

    '{ () }
  }

  inline def showTree(inline expr: Any): Unit = ${ showTreeImpl('expr) }
  def showTreeImpl(expr: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    //println(expr.asTerm.underlyingArgument.showExtractors)
    printer.lnf(expr.asTerm.underlyingArgument)
    '{ () }
  }

  inline def detectPlus(inline expr: Int): (Int, String) = ${ detectPlusImpl('expr) }
  def detectPlusImpl(expr: Expr[Int])(using Quotes): Expr[(Int, String)] = {
    import quotes.reflect._
    println(Printer.TreeStructure.show(expr.asTerm.underlyingArgument))

    val message = 
      expr.asTerm.underlyingArgument.asExpr match {
        case '{ ($one: Int).+(($two: Int)) } => 
          val oneInner = one.asTerm match { case Ident(value) => value }
          val twoInner = two.asTerm match { case Ident(value) => value }

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
    val somethingMakingBool = somethingMakingStringBoolRaw.asTerm.underlyingArgument.asExpr

    val theExpressionAst = Printer.TreeStructure.show(somethingMakingBool.asTerm)
    val theExpressionCode = somethingMakingBool.asTerm.show
    '{
      if ($somethingMakingStringBoolRaw) 
        "Yay, we passed!... " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
      else 
        "We did not pass: " + ${Expr(theExpressionCode)} + "\nOtherwise Known as: " + ${Expr(theExpressionAst)}
    }
  }
}


