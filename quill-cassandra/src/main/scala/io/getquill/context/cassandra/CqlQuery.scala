package io.getquill.context.cassandra

import io.getquill.ast._
import io.getquill.util.Messages.fail
import io.getquill.norm.ApplyMap

case class CqlQuery(
  entity:   Entity,
  filter:   Option[Ast],
  orderBy:  List[OrderByCriteria],
  limit:    Option[Ast],
  select:   List[Ast],
  distinct: Boolean
)

case class OrderByCriteria(
  property: Property,
  ordering: PropertyOrdering
)

object CqlQuery {

  extension (cc: CaseClass)
    def convertableToTuple =
      cc.values
        .map((k, v) => k)
        .zipWithIndex
        .map((k, i) => (k, i + 1))
        .forall((k, i) => k == s"_${i}")
    def convertToTuple =
      Tuple(cc.values.map((k, v) => v))

  /**
   * Since Elaboration/Materialization given by Elaborator/materializeQueryMeta
   * is via case classes in ProtoQuill versus Scala2-Quill
   * {{
   *   // ProtoQuill Elaboration
   *   query[Person] ->
   *     query[Person].map(p => CC("name" -> p.name, "age" -> p.age))
   *   // Scala2-Quill Elaboration (via materializeQueryMeta)
   *   query[Person] ->
   *     query[Person].map(p => (p.name, p.age))
   * }}
   * In Scala2-Quill CqlQuery expected that ApplyMap would convert these tuple-mapped
   * materialized/elaborated entities:
   * {{
   *   query[Person].distinct.map(p => (p.name, p.age))
   *   // Into these:
   *   query[Person].map(p => (p.name, p.age)).distinct
   * }}
   * However this conversion does not hold for ast.CaseClass mapped entities
   * (although ApplyMap should probably be modified so that it should).
   * For now, do the conversion from ast.CaseClass to ast.Tuple (if possible)
   * and do the ApplyMap here.
   */
  def apply(q: Query): CqlQuery =
    q match {
      case outer @ Map(innerQuery, id, cc @ CaseClass(_)) if (cc.convertableToTuple) =>
        val convertedOutput = Map(innerQuery, id, cc.convertToTuple)
        val possibleApplied: Query = ApplyMap.unapply(convertedOutput).getOrElse(convertedOutput)
        possibleApplied match {
          case Distinct(q: Query) =>
            apply(q, true)
          case other =>
            apply(q, false)
        }
      case Distinct(q: Query) =>
        apply(q, true)
      case other =>
        apply(q, false)
    }

  private def apply(q: Query, distinct: Boolean): CqlQuery =
    q match {
      case Map(q: Query, x, p) =>
        apply(q, select(p), distinct)
      case Aggregation(AggregationOperator.`size`, q: Query) =>
        apply(q, List(Aggregation(AggregationOperator.`size`, Constant.auto(1))), distinct)
      case other =>
        apply(q, List(), distinct)
    }

  private def apply(q: Query, select: List[Ast], distinct: Boolean): CqlQuery =
    q match {
      case Take(q: Query, limit) =>
        apply(q, Some(limit), select, distinct)
      case other =>
        apply(q, None, select, distinct)
    }

  private def apply(q: Query, limit: Option[Ast], select: List[Ast], distinct: Boolean): CqlQuery =
    q match {
      case SortBy(q: Query, x, p, o) =>
        apply(q, orderByCriterias(p, o), limit, select, distinct)
      case other =>
        apply(q, List(), limit, select, distinct)
    }

  private def apply(q: Query, orderBy: List[OrderByCriteria], limit: Option[Ast], select: List[Ast], distinct: Boolean): CqlQuery =
    q match {
      case Filter(q: Query, x, p) =>
        apply(q, Some(p), orderBy, limit, select, distinct)
      case other =>
        apply(q, None, orderBy, limit, select, distinct)
    }

  private def apply(q: Query, filter: Option[Ast], orderBy: List[OrderByCriteria], limit: Option[Ast], select: List[Ast], distinct: Boolean): CqlQuery =
    q match {
      case q: Entity =>
        new CqlQuery(q, filter, orderBy, limit, select, distinct)
      case (_: FlatMap) =>
        fail(s"Cql doesn't support flatMap.")
      case (_: Union) | (_: UnionAll) =>
        fail(s"Cql doesn't support union/unionAll.")
      case Join(joinType, _, _, _, _, _) =>
        fail(s"Cql doesn't support $joinType.")
      case _: GroupBy =>
        fail(s"Cql doesn't support groupBy.")
      case q =>
        fail(s"Invalid cql query: $q (ast: ${io.getquill.util.Messages.qprint(q)})")
    }

  private def select(ast: Ast): List[Ast] =
    ast match {
      case Tuple(values) => values.flatMap(select)
      case CaseClass(values) => values.map((k, v) => v).flatMap(select)
      case p: Property   => List(p)
      case i: Ident      => List()
      case l: Lift       => List(l)
      case l: ScalarTag  => List(l)
      case other         => fail(s"Cql supports only properties as select elements. Found: $other (the AST is: ${io.getquill.util.Messages.qprint(other)})")
    }

  private def orderByCriterias(ast: Ast, ordering: Ast): List[OrderByCriteria] =
    (ast, ordering) match {
      case (Tuple(properties), ord: PropertyOrdering) => properties.flatMap(orderByCriterias(_, ord))
      case (Tuple(properties), TupleOrdering(ord))    => properties.zip(ord).flatMap { case (a, o) => orderByCriterias(a, o) }
      case (a: Property, o: PropertyOrdering)         => List(OrderByCriteria(a, o))
      case other                                      => fail(s"Invalid order by criteria $ast")
    }
}
