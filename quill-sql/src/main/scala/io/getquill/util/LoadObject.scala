package io.getquill.util

import scala.reflect.ClassTag
import scala.util.Try
import scala.quoted._

// TODO Note that this does not seem to work when .type is used directly.
// For example, in Dsl.scala, I tried using BaseParserFactory.type
// but then created a delegate trait BaseParsreFactory for the object BaseParseFactory
// which I then used directly. This worked while BaseParseFactory.type did not.
object LoadModule {
  private def `endWith$`(str: String) =
    if (str.endsWith("$")) str else str + "$"

  def applyClassTag[T](implicit tag: ClassTag[T]): Try[T] =
    Try {
      val cls = Class.forName(`endWith$`(tag.runtimeClass.getName))
      val field = cls.getField("MODULE$")
      field.get(cls).asInstanceOf[T]
    }

  object TypeRepr {
    def apply(using Quotes)(loadClassType: quotes.reflect.TypeRepr): Try[Object] = {
      import quotes.reflect.{Try => _, TypeRepr => TTypeRepr, _}
      Try {

        // if (TypeRepr.of[T].classSymbol.isEmpty) {
        //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T]} *** ~~~~~~~~~~~~~~~~~")
        //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol} *** ~~~~~~~~~~~~~~~~~")
        //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol.moduleClass.fullName} *** ~~~~~~~~~~~~~~~~~")
        //   println(s"~~~~~~~~~~~~~~~~~ EMPTY SYMBOL FOR: ${TypeRepr.of[T].termSymbol.companionClass.fullName} *** ~~~~~~~~~~~~~~~~~")
        // }
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
                report.throwError(s"The class ${Format.TypeRepr(loadClassType.widen)} cannot be loaded because it not a static module. Either it is a class or some other dynamic value.")
              }
          }

        // println(s"================== Class Symbol: ${optClassSymbol} ================")
        // println(s"================== Class Symbol FullName: ${optClassSymbol.map(_.fullName)} ================")
        // println(s"================== Class Name: ${className} ================")

        val clsFullRaw = `endWith$`(className)

        // TODO This is a hack! Need to actually use scala compile-time tpe.memberType(tpe.owner) over and over
        // again to get the actual static-lineage until we get to the package name and then compute the name from that
        // Replace io.getquill.Foo$.Bar$ with io.getquill.Foo$Bar which is the java convention for nested modules
        val clsFull = clsFullRaw.replace("$.", "$")

        // println(s"================== Loading Class: ${clsFull} ================")
        val cls = Class.forName(clsFull)
        val field = cls.getField("MODULE$")
        field.get(cls)
      }
    }
  }

  def apply[T: Type](using Quotes): Try[T] = {
    import quotes.reflect.{ TypeRepr => TTypeRepr, _ }
    val loadClassType = TTypeRepr.of[T]
    val tryLoad = TypeRepr(loadClassType)
    tryLoad.map(_.asInstanceOf[T])
  }
}

// TODO Move this to a test
inline def loadMac[T]: String = ${ loadMacImpl[T] }
def loadMacImpl[T: Type](using Quotes): Expr[String] = {
  val loaded = LoadModule[T]
  println( loaded )
  Expr(loaded.toString)
}
