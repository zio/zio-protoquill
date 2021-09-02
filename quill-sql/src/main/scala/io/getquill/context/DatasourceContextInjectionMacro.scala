package io.getquill.context

import scala.quoted._

object DatasourceContextInjectionMacro {
  // If the datasource context is supposed to be injected do that, otherwise pull it in as a variable
  inline def apply[DCI <: DatasourceContextInjection, DatasourceContext, Ctx](inline memberDc: DatasourceContext): DatasourceContext =
    ${ applyImpl[DCI, DatasourceContext, Ctx]('memberDc) }

  def applyImpl[DCI <: DatasourceContextInjection: Type, DatasourceContext: Type, Ctx: Type](memberDc: Expr[DatasourceContext])(using quotes: Quotes): Expr[DatasourceContext] = {
    import quotes.reflect._
    val dciType = TypeRepr.of[DCI]
    if (dciType <:< TypeRepr.of[DatasourceContextInjection.Implicit])
      Expr.summon[DatasourceContext] match
        case Some(dc) =>
          //println(s"============ Using Summoned DataSource from context =========")
          dc
        case None =>
          report.throwError(s"Cannot find implicit data-source '${Printer.TypeReprCode.show(TypeRepr.of[DatasourceContext])}'")
    else {
      memberDc
    }
  }
}
