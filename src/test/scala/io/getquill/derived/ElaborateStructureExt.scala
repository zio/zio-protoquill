package io.getquill.derived

import scala.quoted._
import io.getquill.ast.{ Ast, Map => AMap }
import io.getquill.derived.ElaborateStructure.TaggedLiftedCaseClass
import io.getquill.parser.Lifter

/** Testing Hook for ElaborateStructure */
object ElaborateStructureExt {

  /** An external hook to run the Elaboration with a given AST during runtime (mostly for testing). */
  inline def external[T](ast: Ast): AMap = ${ ElaborateStructure.ontoDynamicAst[T]('ast) }

  inline def ofProductValueExternal[T](productValue: T): TaggedSplicedCaseClass = 
    ${ ElaborateStructureExt.ofProductValueExternalImpl('productValue) }

  
  inline def entityValues[T <: Product](entity: T): List[(String, Any)] = ${ entityValuesImpl('entity) }
  def entityValuesImpl[T <: Product: Type](entity: Expr[T])(using qctx: Quotes): Expr[List[(String, Any)]] = {
    import ElaborateStructure._
    val schema = ElaborateStructure.base[T](Term("x", Branch))
    val elaboration = DeconstructElaboratedEntity(schema, entity)
    val out = elaboration.map((v, k) => (k, v))
    val outExpr = out.map((str, expr) => '{ (${Expr(str)}, $expr) })
    Expr.ofList(outExpr)
  }

  def ofProductValueExternalImpl[T: Type](productValue: Expr[T])(using qctx: Quotes): Expr[TaggedSplicedCaseClass] = {
    import qctx.reflect._
    val tlcc = ElaborateStructure.ofProductValue[T](productValue)
    val liftedLifts = tlcc.lifts.map((str, lift) => '{ ((${Expr(str)}, $lift)) })
    '{ TaggedSplicedCaseClass(${Lifter(tlcc.caseClass)}, ${Expr.ofList(liftedLifts)}) }    
  }

  case class TaggedSplicedCaseClass(ast: Ast, lifts: List[(String, Any)])
}