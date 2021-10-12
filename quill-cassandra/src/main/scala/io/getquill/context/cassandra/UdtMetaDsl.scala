package io.getquill.context.cassandra

import scala.language.experimental.macros
import io.getquill.Udt

object UdtMetaDsl {

  /**
   * Creates udt meta to override udt name / keyspace and rename columns
   * (unlike in Scala2-Quill, any existing QueryMetas are ignored).
   * TODO Make not of that in migration guide.
   *
   * @param path - either `udt_name` or `keyspace.udt_name`
   * @param columns - columns to rename
   * @return udt meta
   */
  inline def udtMeta[T <: Udt](inline path: String, inline columns: (T => (Any, String))*): UdtMeta[T] = ${ UdtMetaDslMacro[T]('path, 'columns) }
}

trait UdtMeta[T <: Udt] {
  def keyspace: Option[String]
  def name: String
  def alias(col: String): Option[String]
}

object UdtMeta:
  import scala.quoted._
  def build[T <: Udt: Type](using Quotes): Expr[UdtMeta[T]] =
    import quotes.reflect._
    if (TypeRepr.of[T] =:= TypeRepr.of[Udt])
      // TODO quill.trace.types 'summoning' level should enable this
      //println("Cannot derive schema for the base Udt (print the stack trace too)")
      '{ ??? }
    else
      Expr.summon[UdtMeta[T]] match
        // if there is an implicit meta
        case Some(meta) => meta
        // def apply[T <: Udt: Type](path: Expr[String], columns: Expr[Seq[T => (Any, String)]])(using Quotes): Expr[UdtMeta[T]] = {
        case None =>
          val typeName = TypeRepr.of[T].widen.typeSymbol.name
          // TODO quill.trace.types 'summoning' level should enable this
          //println(s"MetaDsl not found. Making one with the type name: ${typeName}")
          UdtMetaDslMacro[T](Expr(typeName), Expr.ofList(Seq()))
