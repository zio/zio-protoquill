package io.getquill.record

import io.getquill.Spec
import io.getquill.ast._
import io.getquill.ast.Visibility.Visible
import io.getquill.quat.Quat

case class Person(name: String, age: Int)

class TypedQuerySpec extends Spec {

  val personEntity = Entity("Person", List(), Quat.LeafProduct("Person", "name", "age").probit)

  def prop(alias: String, field: String): Property =
    Property.Opinionated(Ident(alias, Quat.Generic), field, Renameable.neutral, Visible)

  "TypedEntityQuery" - {

    "produces correct Entity AST with entity name" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      teq.entityName mustEqual "Person"
      teq.entityAst mustBe a[Entity]
      teq.entityAst.asInstanceOf[Entity].name mustEqual "Person"
    }

    "filter produces Filter AST with correct Property/BinaryOp nodes" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val filtered = teq.filter(r => r.selectDynamic("name").asInstanceOf[FieldExpr] === "Joe")
      filtered.entityAst mustBe a[Filter]
      val filter = filtered.entityAst.asInstanceOf[Filter]
      filter.body mustEqual BinaryOperation(
        prop("x", "name"),
        EqualityOperator.`_==`,
        Constant("Joe", Quat.Value)
      )
    }

    "map produces Map AST with correct Property node" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val mapped = teq.map(r => r.selectDynamic("name").asInstanceOf[FieldExpr])
      mapped.entityAst mustBe a[Map]
      val mapAst = mapped.entityAst.asInstanceOf[Map]
      mapAst.body mustEqual prop("x", "name")
    }

    "sortBy produces SortBy AST" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val sorted = teq.sortBy(r => r.selectDynamic("age").asInstanceOf[FieldExpr])
      sorted.entityAst mustBe a[SortBy]
      val sortBy = sorted.entityAst.asInstanceOf[SortBy]
      sortBy.criteria mustEqual prop("x", "age")
      sortBy.ordering mustEqual AscNullsFirst
    }

    "take produces Take AST node" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val taken = teq.take(10)
      taken.entityAst mustBe a[Take]
      val take = taken.entityAst.asInstanceOf[Take]
      take.n mustEqual Constant(10, Quat.Value)
    }

    "drop produces Drop AST node" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val dropped = teq.drop(5)
      dropped.entityAst mustBe a[Drop]
      val drop = dropped.entityAst.asInstanceOf[Drop]
      drop.n mustEqual Constant(5, Quat.Value)
    }

    "toQuoted produces a valid Quoted[EntityQuery[T]]" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val quoted = teq.toQuoted
      quoted.ast mustEqual personEntity
      quoted.lifts mustEqual List()
      quoted.runtimeQuotes mustEqual List()
    }

    "Record field access returns FieldExpr with correct alias and field name" in {
      val rec = new Record[Person, Col]("Person", "p")
      val result = rec.selectDynamic("name")
      result mustBe a[FieldExpr]
      val field = result.asInstanceOf[FieldExpr]
      field.entityAlias mustEqual "p"
      field.fieldName mustEqual "name"
    }

    "combined operations chain correctly" in {
      val teq = new TypedEntityQuery[Person](
        "Person",
        personEntity
      )
      val chained = teq
        .filter(r => r.selectDynamic("name").asInstanceOf[FieldExpr] === "Joe")
        .sortBy(r => r.selectDynamic("age").asInstanceOf[FieldExpr])
        .take(10)

      // Outermost is Take
      chained.entityAst mustBe a[Take]
      val take = chained.entityAst.asInstanceOf[Take]
      // Inside Take is SortBy
      take.query mustBe a[SortBy]
      val sortBy = take.query.asInstanceOf[SortBy]
      // Inside SortBy is Filter
      sortBy.query mustBe a[Filter]
      val filter = sortBy.query.asInstanceOf[Filter]
      // Inside Filter is the original Entity
      filter.query mustEqual personEntity
    }
  }
}
