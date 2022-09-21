package io.getquill.context.cassandra

import scala.quoted._
import io.getquill.metaprog.Extractors._
import io.getquill.context.cassandra.util.UdtMetaUtils
import io.getquill.util.Format
import io.getquill.{Udt, UdtMeta}

object UdtMetaDslMacro {

  /**
   * TODO Eventually we might want to use a Naming schema `default` method to look up UDT object names.
   * Maybe should have summonable implicit Naming instance here? (probably would only need the Expr[Naming]
   * since we don't need to directly call it on columns hence wouldn't need to deal with compile-time/runtime
   * dichotomies)
   */
  def apply[T <: Udt: Type](path: Expr[String], columns: Expr[Seq[T => (Any, String)]])(using Quotes): Expr[UdtMeta[T]] = {
    import quotes.reflect._

    val columnsList =
      columns match
        case '{ Nil } => Nil
        case '{ List() } => Nil
        case Varargs(cols) => cols
        case _ => report.throwError(s"Invalid UdtMeta columns list: ${Format.Expr(columns)}", columns)

    // Do we need to do asTerm.underlyingArgument.asExpr to the terms here? As far as I understand,
    // it is not a good idea to splice trees back in that have been underlyingArgumented (check conversations with Stucki)
    // .map(_.asTerm.underlyingArgument.asExpr)
    val pairs = columnsList.map {
      case Lambda1(ident, identTpe, ArrowFunction(prop, ConstExpr(value: String))) =>
        val propName: String = prop.asTerm.symbol.name
        '{ (${Expr(propName: String)}, ${Expr(value: String)}) }
      case other =>
        report.throwError(s"Invalid UdtMeta assignment statement: ${Format.Expr(other)}", other)
    }
    '{
      new UdtMeta[T] {
        val (nm, ks) = UdtMetaUtils.parse(${path}) // TODO Insert `naming.default` here in future
        val map = Map[String, String](${Varargs(pairs)}: _*)
        def name = nm
        def keyspace = ks
        def alias(col: String) = map.get(col)
      }
    }
  }
}
