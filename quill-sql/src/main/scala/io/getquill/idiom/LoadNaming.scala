package io.getquill.idiom

import scala.util.Try

import scala.quoted.{Type => TType, _}

import io.getquill.NamingStrategy
import io.getquill.util.CollectTry
import io.getquill.util.Load
import io.getquill.CompositeNamingStrategy
import scala.util.Success
import scala.util.Failure
import io.getquill.util.Format
import io.getquill.util.CommonExtensions.Option._

object LoadNaming {

  def static[T: TType](using Quotes): Try[NamingStrategy] = {
    import quotes.reflect.{Try => _, _}

    def `endWith$`(str: String) =
      if (str.endsWith("$")) str else str + "$"

    def loadFromTastyType[T](tpe: TypeRepr): Try[T] = {
      val loadClassType = tpe
      for {
        optClassSymbol <- Try(loadClassType.classSymbol)
        className <- Try {
          optClassSymbol match {
            case Some(value) => Success(value.fullName)
            case None =>
              if (!loadClassType.termSymbol.moduleClass.isNoSymbol)
                Success(loadClassType.termSymbol.moduleClass.fullName)
              else
                Failure(new IllegalArgumentException(s"The class ${loadClassType.show} cannot be loaded because it is not a scala class or module"))
          }
        }.flatten
        field <- Try {
          val clsFull = `endWith$`(className)
          val cls = Class.forName(clsFull)
          val field = cls.getField("MODULE$")
          field.get(cls).asInstanceOf[T]
        }
      } yield (field)
    }

    CollectTry {
      strategies[T].map(loadFromTastyType[NamingStrategy](_))
    }.map(NamingStrategy(_))
  } // end static

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
    // println( loadedStrategies )
    Expr(loadedStrategies.toString) // maybe list of string?
  }
}

inline def macLoadNamingStrategy[T](t: T): String = ${ macLoadNamingStrategyImpl[T]('t) }
def macLoadNamingStrategyImpl[T: TType](t: Expr[T])(using Quotes): Expr[String] = {
  import quotes.reflect._
  val loadedStrategies = LoadNaming.static[T]
  // println( loadedStrategies )
  Expr(loadedStrategies.toString) // maybe list of string?
}
