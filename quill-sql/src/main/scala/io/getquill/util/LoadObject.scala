package io.getquill.util

import scala.reflect.ClassTag
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.quoted._
import io.getquill.util.Messages.TraceType
import io.getquill.metaprog.SummonTranspileConfig

// TODO Note that this does not seem to work when .type is used directly.
// For example, in Dsl.scala, I tried using BaseParserFactory.type
// but then created a delegate trait BaseParseFactory for the object BaseParseFactory
// which I then used directly. This worked while BaseParseFactory.type did not.
object Load:

  private def `endWith$`(str: String) =
    if (str.endsWith("$")) str else str + "$"

  private[Load] sealed trait SymbolLoadType { def path: String }
  private[Load] object SymbolLoadType:
    case class Class(path: String) extends SymbolLoadType
    case class Module(path: String) extends SymbolLoadType

  object Module:
    def fromClassTag[T](implicit tag: ClassTag[T]): Try[T] =
      Try {
        val cls = java.lang.Class.forName(`endWith$`(tag.runtimeClass.getName))
        val field = cls.getField("MODULE$")
        field.get(cls).asInstanceOf[T]
      }

    def fromTypeRepr(using Quotes)(loadClassType: quotes.reflect.TypeRepr): Try[Object] =
      import quotes.reflect.{Try => _, TypeRepr => TTypeRepr, _}
      for {
        sym <- symbolType(loadClassType)
        objectLoad <-
          Try {
            val className = sym.path
            val clsFullRaw = `endWith$`(className)

            // TODO This is a hack! Need to actually use scala compile-time tpe.memberType(tpe.owner) over and over
            // again to get the actual static-lineage until we get to the package name and then compute the name from that
            // Replace io.getquill.Foo$.Bar$ with io.getquill.Foo$Bar which is the java convention for nested modules
            val clsFull = clsFullRaw.replace("$.", "$")

            val cls = java.lang.Class.forName(clsFull)
            val field = cls.getField("MODULE$")
            field.get(cls)
          }
      } yield objectLoad

    def apply[T: Type](using Quotes): Try[T] =
      import quotes.reflect.{TypeRepr => TTypeRepr, _}
      val tryLoad = fromTypeRepr(TTypeRepr.of[T])
      tryLoad.map(_.asInstanceOf[T])
  end Module

  object Class:
    def fromTypeRepr(using Quotes)(loadClassType: quotes.reflect.TypeRepr): Try[java.lang.Class[_]] =
      for {
        symLoad <- symbolType(loadClassType)
        sym <-
          symLoad match
            case SymbolLoadType.Module(path) =>
              Failure(throw new IllegalArgumentException(s"${Format.TypeRepr(loadClassType)} must not be a class type because it has no class symbol."))
            case SymbolLoadType.Class(path) =>
              Success(path)

        objectLoad <- Try(java.lang.Class.forName(sym))
      } yield objectLoad

  private[Load] def symbolType(using Quotes)(loadClassType: quotes.reflect.TypeRepr): Try[SymbolLoadType] =
    val traceConfig = SummonTranspileConfig().traceConfig
    val interp = new Interpolator(TraceType.Warning, traceConfig, 1)
    import interp._
    Try {
      loadClassType.classSymbol match
        case Some(value) =>
          Success(SymbolLoadType.Class(value.fullName))
        case None =>
          trace"${Format.TypeRepr(loadClassType)} must not be a class type because it has no class symbol. Attempting to load it as a module.".andLog()
          if (!loadClassType.termSymbol.moduleClass.isNoSymbol)
            Success(SymbolLoadType.Module(loadClassType.termSymbol.moduleClass.fullName))
          else
            Failure(new IllegalArgumentException(s"The class ${Format.TypeRepr(loadClassType.widen)} cannot be loaded because it not a static module. Either it is a class or some other dynamic value."))
    }.flatten

end Load

// TODO Move this to a test
inline def loadMac[T]: String = ${ loadMacImpl[T] }
def loadMacImpl[T: Type](using Quotes): Expr[String] = {
  val loaded = Load.Module[T]
  println(loaded)
  Expr(loaded.toString)
}
