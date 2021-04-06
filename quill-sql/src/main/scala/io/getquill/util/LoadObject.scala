package io.getquill.util

import scala.reflect.ClassTag
import scala.util.Try
import scala.quoted._

// TODO Note that this does not seem to work when .type is used directly.
// For example, in Dsl.scala, I tried using BaseParserFactory.type
// but then created a delegate trait BaseParsreFactory for the object BaseParseFactory
// which I then used directly. This worked while BaseParseFactory.type did not.
object LoadObject {
  private def `endWith$`(str: String) =
    if (str.endsWith("$")) str else str + "$"

  def applyClassTag[T](implicit tag: ClassTag[T]): Try[T] =
    Try {
      val cls = Class.forName(`endWith$`(tag.runtimeClass.getName))
      val field = cls.getField("MODULE$")
      field.get(cls).asInstanceOf[T]
    }

  def apply[T: Type](using Quotes): Try[T] = {
    import quotes.reflect.{Try => _, _}
    Try {
      
      // if (TypeRepr.of[T].classSymbol.isEmpty) {
      //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T]} *** ~~~~~~~~~~~~~~~~~")  
      //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol} *** ~~~~~~~~~~~~~~~~~")  
      //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol.moduleClass.fullName} *** ~~~~~~~~~~~~~~~~~")  
      //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol.companionClass.fullName} *** ~~~~~~~~~~~~~~~~~")  
      // }
      val loadClassType = TypeRepr.of[T]
      val optClassSymbol = loadClassType.classSymbol
      val className = 
        optClassSymbol match {
          case Some(value) => value.fullName
          case None =>
            //println(s"${'[$tpe].show} is not a class type. Attempting to load it as a module.")
            if (!loadClassType.termSymbol.moduleClass.isNoSymbol) {
              loadClassType.termSymbol.moduleClass.fullName
            } else {
              //println(s"The class ${'[$tpe].show} cannot be loaded because it is either a scala class or module")
              report.throwError(s"The class ${Type.show[T]} cannot be loaded because it is either a scala class or module")
            }
        }

      val clsFull = `endWith$`(className)
      val cls = Class.forName(clsFull)
      val field = cls.getField("MODULE$")
      field.get(cls).asInstanceOf[T]
    }
  }
}

// TODO Move this to a test
inline def loadMac[T]: String = ${ loadMacImpl[T] }
def loadMacImpl[T: Type](using Quotes): Expr[String] = {
  val loaded = LoadObject[T]
  println( loaded )
  Expr(loaded.toString)
}
