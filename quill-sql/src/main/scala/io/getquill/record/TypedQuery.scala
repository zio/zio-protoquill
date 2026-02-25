package io.getquill.record

import scala.quoted._
import scala.deriving.Mirror
import io.getquill.ast._
import io.getquill.quat.Quat
import io.getquill.{Quoted, EntityQuery, Query}

/**
 * A typed query wrapper that uses Record[T, Col] for field access.
 *
 * Produces the same AST nodes as the macro-based `query[T]` API, but derives
 * schema information via NamedTuple at the type level rather than through
 * heavy macro AST inspection.
 *
 * Users can opt in to this API for better compile times while the existing
 * `query[T]` continues working unchanged.
 */
class TypedEntityQuery[T](
    val entityName: String,
    val entityAst: Ast
) {

  /**
   * Create a Record[T, Col] for use in lambda bodies with structural field access.
   */
  def record(alias: String): Record[T, Col] = new Record[T, Col](entityName, alias)

  /**
   * Apply a filter predicate using Record-based field access.
   * The lambda receives a Record[T, Col] whose field accesses produce FieldExpr values.
   */
  def filter(f: Record[T, Col] => FilterExpr): TypedEntityQuery[T] = {
    val alias = "x"
    val rec = record(alias)
    val filterExpr = f(rec)
    val newAst = Filter(entityAst, Ident(alias, Quat.Generic), filterExpr.ast)
    new TypedEntityQuery[T](entityName, newAst)
  }

  /**
   * Apply a map operation using Record-based field access.
   *
   * Note: Currently returns `TypedEntityQuery[T]` (the original entity type) rather than
   * changing the result type to match the projection. This means map only supports
   * column selection/reordering within the same entity — it modifies the SQL AST but
   * the decoder still targets `T`. Type-changing projections (e.g., mapping to a different
   * case class) require a `Queryable[Q, R]` typeclass approach; see the architecture
   * document for the planned rearchitecture.
   */
  def map(f: Record[T, Col] => FieldExpr): TypedEntityQuery[T] = {
    val alias = "x"
    val rec = record(alias)
    val fieldExpr = f(rec)
    val newAst = Map(entityAst, Ident(alias, Quat.Generic), fieldExpr.toAst)
    new TypedEntityQuery[T](entityName, newAst)
  }

  /**
   * Apply a sort-by operation.
   */
  def sortBy(f: Record[T, Col] => FieldExpr): TypedEntityQuery[T] = {
    val alias = "x"
    val rec = record(alias)
    val fieldExpr = f(rec)
    val newAst = SortBy(entityAst, Ident(alias, Quat.Generic), fieldExpr.toAst, AscNullsFirst)
    new TypedEntityQuery[T](entityName, newAst)
  }

  /**
   * Apply a take (LIMIT) operation.
   */
  def take(n: Int): TypedEntityQuery[T] = {
    new TypedEntityQuery[T](entityName, Take(entityAst, Constant(n, Quat.Value)))
  }

  /**
   * Apply a drop (OFFSET) operation.
   */
  def drop(n: Int): TypedEntityQuery[T] = {
    new TypedEntityQuery[T](entityName, Drop(entityAst, Constant(n, Quat.Value)))
  }

  /**
   * Convert to a Quoted[EntityQuery[T]] for use with existing ctx.run() infrastructure.
   * This bridges the new Record-based API into the existing execution pipeline.
   */
  def toQuoted: Quoted[EntityQuery[T]] = {
    Quoted[EntityQuery[T]](entityAst, List(), List())
  }
}


/**
 * Macro that creates a TypedEntityQuery with the entity name derived from the type parameter.
 */
object TypedQueryMacro {
  def apply[T: Type](using Quotes): Expr[TypedEntityQuery[T]] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[T]
    val entityName = tpe.typeSymbol.name
    '{
      new TypedEntityQuery[T](
        ${ Expr(entityName) },
        Entity(${ Expr(entityName) }, List(), io.getquill.quat.QuatMaking.inferQuatType[T].probit)
      )
    }
  }
}
