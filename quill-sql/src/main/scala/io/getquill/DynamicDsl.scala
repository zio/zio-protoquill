package io.getquill

import io.getquill.ast.Ast
import io.getquill.ast.ScalarTag.apply
import java.util.UUID
import io.getquill.ast.ScalarTag
import io.getquill.ast.External
import io.getquill.generic.GenericEncoder
import io.getquill.ast.Ident
import io.getquill.quat.Quat
import scala.util.DynamicVariable
import io.getquill.quat.QuatMaking
import io.getquill.ast.Entity
import io.getquill.ast.Property
import scala.annotation.tailrec
import io.getquill.ast.PropertyAlias
import io.getquill.ast.Renameable.Fixed
import io.getquill.context.UnquoteMacro
import io.getquill.{Action => DslAction}
import io.getquill.metaprog.Extractors

implicit class ToDynamicQuery[T](q: Quoted[Query[T]]) {
  def dynamic: DynamicQuery[T] = DynamicQuery(q)
}

implicit class ToDynamicEntityQuery[T](q: Quoted[EntityQuery[T]]) {
  def dynamic: DynamicEntityQuery[T] = DynamicEntityQuery(q)
}

implicit class ToDynamicAction[T](q: Quoted[DslAction[T]]) {
  def dynamic: DynamicAction[DslAction[T]] = DynamicAction(q)
}

implicit class ToDynamicInsert[T](q: Quoted[Insert[T]]) {
  def dynamic: DynamicInsert[T] = DynamicInsert(q)
}

implicit class ToDynamicUpdate[T](q: Quoted[Update[T]]) {
  def dynamic: DynamicUpdate[T] = DynamicUpdate(q)
}

implicit class ToDynamicActionReturning[T, U](
    q: Quoted[ActionReturning[T, U]]
) {
  def dynamic: DynamicActionReturning[T, U] = DynamicActionReturning(q)
}

private[getquill] def dyn[T](ast: Ast, lifts: List[Planter[_, _, _]], runtimeQuotes: List[QuotationVase]): DynamicQuery[T] =
  DynamicQuery[T](Quoted[Query[T]](ast, lifts, runtimeQuotes))

private[getquill] def spliceLift[O](o: O, otherLifts: List[Planter[_, _, _]], runtimeQuotes: List[QuotationVase])(implicit enc: GenericEncoder[O, _, _]) = {
  val uid = UUID.randomUUID().toString + "foo"
  new Quoted[O](ScalarTag(uid, External.Source.Parser), EagerPlanter(o, enc, uid) +: otherLifts, runtimeQuotes)
}

private[getquill] object DynamicDslModel {
  val nextIdentId = new DynamicVariable(0)
}

private[getquill] def withFreshIdent[R](f: Ident => R)(quat: Quat): R = {
  val idx = DynamicDslModel.nextIdentId.value
  DynamicDslModel.nextIdentId.withValue(idx + 1) {
    f(Ident(s"v$idx", quat))
  }
}

inline implicit def dynamicUnquote[T](inline d: DynamicQuery[T]): Query[T] =
  ${ UnquoteMacro('{ d.q }) }

def set[T, U](
    property: Quoted[T] => Quoted[U],
    value: Quoted[U]
): DynamicSet[T, U] =
  DynamicSetValue(property, value)

def setValue[T, U](
    property: Quoted[T] => Quoted[U],
    value: U
)(implicit enc: GenericEncoder[U, _, _]): DynamicSet[T, U] =
  set[T, U](property, spliceLift(value, Nil, Nil))

def setOpt[T, U](property: Quoted[T] => Quoted[U], value: Option[U])(
    implicit enc: GenericEncoder[U, _, _]
): DynamicSet[T, U] =
  value match {
    case Some(v) => setValue(property, v)
    case None    => DynamicSetEmpty()
  }

def set[T, U](property: String, value: Quoted[U]): DynamicSet[T, U] =
  set((f: Quoted[T]) => Quoted(Property(f.ast, property), f.lifts, f.runtimeQuotes), value)

def setValue[T, U](
    property: String,
    value: U
)(implicit enc: GenericEncoder[U, _, _]): DynamicSet[T, U] =
  set(property, spliceLift(value, Nil, Nil))

def alias[T](
    property: Quoted[T] => Quoted[Any],
    name: String
): DynamicAlias[T] = DynamicAlias(property, name)

implicit inline def toQuoted[T](inline q: DynamicQuery[T]): Quoted[Query[T]] = q.q
implicit inline def toQuoted[T](inline q: DynamicEntityQuery[T]): Quoted[EntityQuery[T]] = q.q
implicit inline def toQuoted[T <: DslAction[_]](inline q: DynamicAction[T]): Quoted[T] = q.q

inline def dynamicQuery[T]: DynamicEntityQuery[T] = {
  DynamicEntityQuery(
    Quoted[EntityQuery[T]](
      Entity(Extractors.typeName[T], Nil, QuatMaking.inferQuatType[T].probit),
      Nil,
      Nil
    )
  )
}

inline def dynamicQuerySchema[T](
    entity: String,
    columns: DynamicAlias[T]*
): DynamicEntityQuery[T] = {
  val aliasesAndProperties =
    columns.map { alias =>
      @tailrec def path(ast: Ast, acc: List[String] = Nil): List[String] =
        ast match {
          case Property(a, name) =>
            path(a, name :: acc)
          case _ =>
            acc
        }

      val v = alias.property(Quoted[T](Ident("v", QuatMaking.inferQuatType[T].probit), Nil, Nil))
      (v, PropertyAlias(path(v.ast), alias.name))
    }
  val aliases = aliasesAndProperties.map(_._2)
  val lifts = aliasesAndProperties.map(_._1.lifts).flatten.toList
  val runtimeQuotes = aliasesAndProperties.map(_._1.runtimeQuotes).flatten.toList
  DynamicEntityQuery(
    Quoted[EntityQuery[T]](
      Entity.Opinionated(entity, aliases.toList, QuatMaking.inferQuatType[T].probit, Fixed),
      lifts,
      runtimeQuotes
    )
  )
}
