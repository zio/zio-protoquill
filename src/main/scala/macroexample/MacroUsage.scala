package macroexample

object Example {
  var one: String = "foo"
  var two: String = "bar"
}

class SomeClass {
  def method: String = "simple"
}



@main
def doIt = {
  val stuffVar = "stuff"
  def numberOne: Int = 111111
  def numberTwo: Int = 222222
  def numberThree: Int = 3333333
  //println( MacroExample.macroTest(1 == number) )
  //println( MacroExample.detectPlus(numberOne + numberTwo) )

  //MacroExample.showTree({numberOne; numberTwo; numberThree; "blahblah"})


  /*
  Block(
    List(DefDef(foo, List(), Ident(Int), Apply(Select(Ident(v), length), List()), List(List(ValDef(v, Ident(String), Thicket()))))),
    Apply(Select(Literal(("blah")), +), List(Apply(Ident(foo), List(Literal(("somethingg"))))))
  )
  */
  // MacroExample.showTree({def foo(v: String): Int = v.length; "blah" + foo("somethingg")})

  /*
  Block(
    List(
      DefDef(
        $anonfun,
        List(),
        List(List(ValDef(v, Ident(String), Thicket(List())))),
        TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],
        Apply(Select(Ident(v), length), List())
      )
    ),
    Closure(List(), Ident($anonfun), Thicket(List()))
  )
  */
  MacroExample.showTree((v: String) => v.length) //hellooooooooo
  

  //MacroExample.showTreeMatchLambda((v: String, vv: String) => v.length)
  
  //MacroExample.getMethods("123")
}