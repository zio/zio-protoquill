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
  //println( MacroExample.macroTest(1 == number) )
  println( MacroExample.detectPlus(numberOne + numberTwo) )


}