package io.getquill.idiom

import scala.util.Try

import scala.quoted.{Type => TType, _}

import io.getquill.NamingStrategy
import io.getquill.util.CollectTry
import io.getquill.util.LoadModule
import io.getquill.CompositeNamingStrategy

object LoadNaming {

  def static[T: TType](using Quotes): Try[NamingStrategy] = {
    import quotes.reflect.{Try => _, _}

    def `endWith$`(str: String) =
      if (str.endsWith("$")) str else str + "$"

    def loadFromTastyType[T](tpe: TypeRepr): Try[T] =
      Try {
        val loadClassType = tpe
        val optClassSymbol = loadClassType.classSymbol
        val className =
          optClassSymbol match {
            case Some(value) => value.fullName
            case None =>
              //println(s"${loadClassType.show} is not a class type. Attempting to load it as a module.")
              if (!loadClassType.termSymbol.moduleClass.isNoSymbol) {
                loadClassType.termSymbol.moduleClass.fullName
              } else {
                //println(s"The class ${loadClassType.show} cannot be loaded because it is either a scala class or module")
                report.throwError(s"The class ${loadClassType.show} cannot be loaded because it is either a scala class or module")
              }
          }
        val clsFull = `endWith$`(className)
        val cls = Class.forName(clsFull)
        val field = cls.getField("MODULE$")
        field.get(cls).asInstanceOf[T]
      }

    CollectTry {
      strategies[T].map(loadFromTastyType[NamingStrategy](_))
    }.map(NamingStrategy(_))
  }

  private def strategies[T: TType](using Quotes) = {
    import quotes.reflect._
    val treeTpe = TypeRepr.of[T]
    treeTpe <:< TypeRepr.of[CompositeNamingStrategy] match {
      case true =>
        treeTpe match {
          case AppliedType(_, types) =>
            types
              .filter(_.isInstanceOf[TypeRepr]).map(_.asInstanceOf[TypeRepr])
              .filterNot(_ =:= TypeRepr.of[NamingStrategy])
              .filterNot(_ =:= TypeRepr.of[Nothing])
        }
      case false =>
        List(treeTpe)
    }
  }

  inline def mac[T](t: T): String = ${ macImpl[T]('t) }
  def macImpl[T: TType](t: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect._
    val loadedStrategies = strategies[T]
    //println( loadedStrategies )
    Expr(loadedStrategies.toString) // maybe list of string?
  }
}


inline def macLoadNamingStrategy[T](t: T): String = ${ macLoadNamingStrategyImpl[T]('t) }
def macLoadNamingStrategyImpl[T: TType](t: Expr[T])(using Quotes): Expr[String] = {
  import quotes.reflect._
  val loadedStrategies = LoadNaming.static[T]
  //println( loadedStrategies )
  Expr(loadedStrategies.toString) // maybe list of string?
}
