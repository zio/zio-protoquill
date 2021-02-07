package io.getquill.derived

import scala.quoted._
import io.getquill.ast.{ Ast, Map => AMap }
import io.getquill.derived.ElaborateStructure.TaggedLiftedCaseClass
import io.getquill.parser.Lifter

/** Testing Hook for ElaborateStructure */
object ElaborateStructureHook {

  /** An external hook to run the Elaboration with a given AST during runtime (mostly for testing). */
  inline def external[T](ast: Ast): AMap = ${ ElaborateStructure.ontoDynamicAst[T]('ast) }

  inline def ofCaseClassExternal[T](baseName: String, claseClassExpression: T): TaggedSplicedCaseClass = 
    ${ ElaborateStructureHook.ofCaseClassExternalImpl('baseName, 'claseClassExpression) }

  
  inline def entityValues[T <: Product](entity: T): List[(String, Any)] = ${ entityValuesImpl('entity) }
  def entityValuesImpl[T <: Product: Type](entity: Expr[T])(using qctx: Quotes): Expr[List[(String, Any)]] = {
    import ElaborateStructure._
    val schema = ElaborateStructure.base[T](Term("x", Branch))
    val elaboration = DeconstructElaboratedEntity(schema, entity)
    val out = elaboration.map((v, k) => (k, v))
    val outExpr = out.map((str, expr) => '{ (${Expr(str)}, $expr) })
    Expr.ofList(outExpr)
  }

  def ofCaseClassExternalImpl[T: Type](baseName: Expr[String], claseClassExpression: Expr[T])(using qctx: Quotes): Expr[TaggedSplicedCaseClass] = {
    import qctx.reflect._
    val baseNameStr = 
      baseName match 
        case Expr(str: String) => str

    val tlcc = ElaborateStructure.ofCaseClassExpression[T](baseNameStr, claseClassExpression)
    val liftedLifts = tlcc.lifts.map((str, lift) => '{ ((${Expr(str)}, $lift)) })
    '{ TaggedSplicedCaseClass(${Lifter(tlcc.caseClass)}, ${Expr.ofList(liftedLifts)}) }    
  }

  case class TaggedSplicedCaseClass(ast: Ast, lifts: List[(String, Any)])
}