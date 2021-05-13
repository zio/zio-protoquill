package io.getquill.generic

import scala.quoted._
import io.getquill.ast.{ Ast, Map => AMap }
import io.getquill.generic.ElaborateStructure.TaggedLiftedCaseClass
import io.getquill.parser.Lifter
import io.getquill.generic.ElaborationSide

/** Testing Hook for ElaborateStructure */
object ElaborateStructureExt {

  /** An external hook to run the Elaboration with a given AST during runtime (mostly for testing). */
  inline def external[T](ast: Ast): AMap = ${ ElaborateStructure.ontoDynamicAst[T]('ast) }

  inline def ofProductValueExternal[T](productValue: T): TaggedSplicedCaseClass =
    ${ ElaborateStructureExt.ofProductValueExternalImpl('productValue) }


  inline def entityValues[T <: Product](entity: T): List[(String, Any)] = ${ entityValuesImpl('entity) }
  private def entityValuesImpl[T <: Product: Type](entity: Expr[T])(using Quotes): Expr[List[(String, Any)]] = {
    import ElaborateStructure._
    // elaboration side is decoding although we might want to add tests to see it from the other side too
    val schema = ElaborateStructure.base[T](Term("x", Branch), ElaborationSide.Decoding)
    val elaboration = DeconstructElaboratedEntity(schema, entity)
    val out = elaboration.map((v, k) => (k, v))
    val outExpr = out.map((str, expr) => '{ (${Expr(str)}, $expr) })
    Expr.ofList(outExpr)
  }

  inline def entityValuesLambda[T <: Product]: List[T => Any] = ${ entityValuesLambdaImpl[T] }
  def entityValuesLambdaImpl[T <: Product: Type](using qctx: Quotes): Expr[List[T => Any]] = {
    import ElaborateStructure._
    val schema = ElaborateStructure.base[T](Term("x", Branch), ElaborationSide.Decoding)
    val fieldGetters = DeconstructElaboratedEntityLevels[T](schema)
    Expr.ofList(fieldGetters)
  }

  // TODO Should test both encoding and decoding side of the elaboration
  // For now this just used on the encoding side in the tests
  private def ofProductValueExternalImpl[T: Type](productValue: Expr[T])(using Quotes): Expr[TaggedSplicedCaseClass] = { 
    val tlcc = ElaborateStructure.ofProductValue[T](productValue, ElaborationSide.Encoding)
    val liftedLifts = tlcc.lifts.map((str, lift) => '{ ((${Expr(str)}, $lift)) })
    '{ TaggedSplicedCaseClass(${Lifter(tlcc.caseClass)}, ${Expr.ofList(liftedLifts)}) }
  }

  case class TaggedSplicedCaseClass(ast: Ast, lifts: List[(String, Any)])
}