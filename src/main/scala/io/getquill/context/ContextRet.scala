package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
//import io.getquill.dsl.Dsl
//import io.getquill.util.Messages.fail
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ ReturnAction }
import miniquill.dsl.EncodingDsl
import miniquill.quoter.Quoted
import miniquill.quoter.MyQuoted
import miniquill.quoter.QueryMeta
import io.getquill.derived._
import miniquill.context.mirror.MirrorDecoders
import miniquill.context.mirror.Row
import miniquill.dsl.GenericDecoder
import miniquill.quoter.ScalarPlanter
import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag
import scala.quoted._
import io.getquill.idiom.Idiom
import io.getquill.ast.{Transform, QuotationTag}
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotedExpr
import miniquill.quoter.ScalarPlanterExpr
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.idiom.{ Idiom => Id }
import io.getquill.{ NamingStrategy => Na }

import io.getquill._

object ContextRet {

  def runTestImpl(quoted: Expr[MyQuoted[_]])(using qctx: Quotes): Expr[String] = {
    '{ $quoted.ast.toString }
  }

  inline def runAndTest[T](inline quoted: MyQuoted[T]): String = 
    ${ runTestImpl('quoted) }
}