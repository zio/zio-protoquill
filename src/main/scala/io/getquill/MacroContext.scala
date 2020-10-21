package io.getquill

import io.getquill.context.Context
import io.getquill.idiom.Idiom
import miniquill.quoter._
import io.getquill.derived._

object MacroContext {
  import scala.quoted._
  

  // inline def runQuery[T, D <: Idiom, N <: NamingStrategy, RQR[_]](ctx: Context[D, N]{ type RunQueryResult[T] = RQR[T] }): RQR[T] = 
  //   ${ runQueryImpl[T, D, N, RQR]('ctx) }

  // def runQueryImpl[T, D <: Idiom, N <: NamingStrategy, RQR[_]](ctx: Expr[Context[D, N]{ type RunQueryResult[T] = RQR[T] }]): Expr[RQR[T]] = {
  //   '{
  //     $ctx.doStuffWithResult[T]("hello")
  //   }
  // }


  // inline def runQuery[T, D <: Idiom, N <: NamingStrategy](quoted: Quoted[Query[T]], ctx: Context[D, N]): ctx.RunQueryResult[T] = 
  //   ${ runQueryImpl('quoted, 'ctx) }

  // def runQueryImpl[T, D <: Idiom, N <: NamingStrategy, Result](
  //   quoted: Expr[Quoted[Query[T]]],
  //   ctx: Expr[Context[D, N]]
  // )(using qctx: QuoteContext): Expr[Result] = {
  //   import qctx.tasty._

  //   val (ast, lifts) =
  //     quoted match {
  //       case '{ Quoted($ast, $lifts) } =>
  //         (ast, lifts)
  //     }

  //   null
  // }
}