package io.getquill

import io.getquill.context.Context
import io.getquill.idiom.Idiom

object MacroContext {
  import scala.quoted._
  import scala.quoted.matching._

  // inline def runQuery[T, D <: Idiom, N <: NamingStrategy, RQR[_]](ctx: Context[D, N]{ type RunQueryResult[T] = RQR[T] }): RQR[T] = 
  //   ${ runQueryImpl[T, D, N, RQR]('ctx) }

  // def runQueryImpl[T, D <: Idiom, N <: NamingStrategy, RQR[_]](ctx: Expr[Context[D, N]{ type RunQueryResult[T] = RQR[T] }]): Expr[RQR[T]] = {
  //   '{
  //     $ctx.doStuffWithResult[T]("hello")
  //   }
  // }


  inline def runQuery[T, D <: Idiom, N <: NamingStrategy](ctx: Context[D, N]): Context[D, N]#RunQueryResult[T] = 
    ${ runQueryImpl[T, D, N]('ctx) }

  def runQueryImpl[T, D <: Idiom, N <: NamingStrategy](ctx: Expr[Context[D, N]]): Expr[Context[D, N]#RunQueryResult[T]] = {
    null
  }
}