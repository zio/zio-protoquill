package io.getquill.context

import scala.quoted._
import io.getquill.StaticSplice
import io.getquill.util.LoadModule
import io.getquill.metaprog.Extractors
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import io.getquill.util.Format

object StaticSpliceMacro {
  import Extractors._

  object SelectPath:
    def recurseInto(using Quotes)(term: quotes.reflect.Term, accum: List[String] = List()): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        // Select(Select(Ident("core"), "foo"), "bar") => recurseInto( {Select(Ident("core"), "foo")}, "bar" +: List("baz") )
        case Select(inner, pathNode) => recurseInto(inner, pathNode +: accum)
        case id: Ident => Some((id, accum))
        // If at the core of the nested selects is not a Ident, this does not match
        case other => None

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        case select: Select => recurseInto(select)
        case _ => None
  end SelectPath

  object ReflectivePathChainLookup:
    import java.lang.reflect.{ Method, Field }
    def singleLookup(obj: Object, path: String): Try[Object] =
      val cls = obj.getClass
      val methodOrField: Try[Method | Field] = Try(cls.getField(path)).orElse(Try(cls.getMethod(path)))
      methodOrField match
        case Success(value) =>
          value match
            case m: Method => Try(m.invoke(obj))
            case f: Field => Try(f.get(obj))
        case Failure(_) =>
          Failure(new IllegalArgumentException(s"A method or field ${path} could not be found on the class ${cls}"))

    def chainLookup(obj: Try[Object], paths: List[String]): Try[Object] =
      paths match
        case Nil => obj
        case head :: tail =>
          obj match
            case Success(value) =>
              val nextLookup = singleLookup(value, head)
              chainLookup(nextLookup, tail)
            case e @ Failure(_) => e
    def apply(obj: Object, paths: List[String]) =
      chainLookup(Success(obj), paths)
  end ReflectivePathChainLookup

  def apply[T: Type](value: Expr[T])(using Quotes): Expr[String] =
    // TODO summon a Expr[StaticSplicer] using the T type passed originally.
    // Then use use LoadModule to get the value of that thing during runtime so we can use it
    // (i.e. see io.getquill.metaprog.SummonParser on how to do that)
    // for primitive types e.g. String, Int, Float etc... rather then making summonable splicers
    // it is easier to just splice them directly, since otherwise those StaticSplicer modules themselves
    // need to be compiled in a previous compilation unit, and we want to define them here.
    // Technically that should be fine because they will only actually be used in the testing code though
    // should think about this more later. For now just do toString to check that stuff from the main return works

    import quotes.reflect._
    Untype(value.asTerm.underlyingArgument) match
      case SelectPath(ident, path) =>
        // TODO Check if ident is a static module, throw an error otherwise
        val module = LoadModule.TypeRepr(ident.tpe.widen)
        module match
          case Success(value) =>
            ReflectivePathChainLookup(value, path) match
              case Success(value) => Expr(value.toString) // TODO Summon StaticSplicer here
              case Failure(e) =>
                report.throwError(s"Could not look up {${Format.TypeRepr(ident.tpe.widen)}}.${path.mkString(".")}. Failed because:\n${e.getMessage}")

          case Failure(_) =>
            // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
            report.throwError(s"Could not load a static value from ${value} since it could not be loaded statically.")
      case _ =>
        // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
        report.throwError(s"Could not load a static value from ${value}")
}