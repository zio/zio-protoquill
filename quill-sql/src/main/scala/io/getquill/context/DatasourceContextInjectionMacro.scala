package io.getquill.context

import scala.quoted._

object DatasourceContextInjectionMacro {
  // If the datasource context is supposed to be injected do that, otherwise pull it in as a variable
  inline def apply[DCI <: RunnerSummoningBehavior, Runner, Ctx](inline memberDc: Runner): Runner =
    ${ applyImpl[DCI, Runner, Ctx]('memberDc) }

  def applyImpl[DCI <: RunnerSummoningBehavior: Type, Runner: Type, Ctx: Type](memberDc: Expr[Runner])(using quotes: Quotes): Expr[Runner] = {
    import quotes.reflect._
    val dciType = TypeRepr.of[DCI]
    if (dciType <:< TypeRepr.of[RunnerSummoningBehavior.Implicit])
      Expr.summon[Runner] match
        case Some(dc) =>
          // println(s"============ Using Summoned DataSource from context =========")
          dc
        case None =>
          report.throwError(s"Cannot find implicit data-source '${Printer.TypeReprCode.show(TypeRepr.of[Runner])}'")
    else {
      memberDc
    }
  }
}
