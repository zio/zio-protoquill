package zio.quill.metaprog

import zio.quill.parser.ParserFactory
import zio.quill.util.Load
import zio.quill.util.Format
import scala.quoted._
import scala.util.Success
import scala.util.Failure
import zio.quill.parser.SerializeQuat
import zio.quill.parser.SerializeAst

object SummonSerializationBehaviors {
  import scala.quoted._

  /** Summon any serialization behavior defined on the context. If it does not exist return None */
  def apply()(using Quotes): (Option[SerializeQuat], Option[SerializeAst]) = {
    import quotes.reflect._
    // Find a SerializationBehavior and try to unlift it
    val serializeAst =
      Expr.summon[SerializeAst] match {
        case Some(value) => Some(SerializeAst.Lifter(value))
        case None        => None
      }
    val serializeQuat =
      Expr.summon[SerializeQuat] match {
        case Some(value) => Some(SerializeQuat.Lifter(value))
        case None        => None
      }
    (serializeQuat, serializeAst)
  }
}

object SummonParser {
  def apply()(using Quotes): ParserFactory = {
    import quotes.reflect._
    Expr.summon[ParserFactory] match {
      case Some(factory) =>
        val factoryTypeRepr = factory.asTerm.tpe.widen
        factoryTypeRepr.asType match {
          case '[t] =>
            Load.Module[t] match {
              case Success(parser) => parser.asInstanceOf[ParserFactory]
              case Failure(e)      => report.throwError(s"Could not summon a parser of type ${Format.TypeOf[t]}. A parser must be a static object created in it's own compilation unit. ${e}")
            }
        }
      case None =>
        report.throwError("Could not summon a parser factory")
    }
  }
}
