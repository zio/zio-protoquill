package io.getquill.dynamic

import io.getquill.{Literal, MirrorContext, PostgresDialect, Query, Quoted, Spec, SuperContext, Update, *}
import org.scalatest.Inside
import io.getquill.ast.*
import io.getquill.context.ExecutionType
import io.getquill.util.Messages.qprint


class DynamicScenariosSpec extends Spec with Inside with SuperContext[PostgresDialect, Literal] {

  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
  import ctx.*

  "Dynamic multi-query scenarios should work" - {
    "List[Quoted[Query]].reduce(_ ++ _)" in {
      val ids = List(1, 2)
      val queries: List[Quoted[Query[Person]]] = ids.map(id => quote {
        query[Person].filter(p => p.id == lift(id))
      })
      val unioned = queries.reduce { case (a, b) => a ++ b }
      val combo = ctx.run(unioned)

      inside(combo.info.ast) {
        case UnionAll(
          Filter(Entity("Person", List(), _), _, BinaryOperation(_, _, tagA: ScalarTag)),
          Filter(Entity("Person", List(), _), _, BinaryOperation(_, _, tagB: ScalarTag))
        ) =>
          if (tagA.uid != tagB.uid) succeed
          else fail(s"The tags of the two entity clauses were the same in: ${qprint(combo.info.ast)}")
      }
    }
    "List[Quoted[Query]].filter.filter..." in {
      val ids = List(1, 2)
      val q: Quoted[Query[Person]] = quote {
        query[Person]
      }
      val filtered = ids.foldLeft(q) { case (qry, id) => qry.filter(q => q.id == lift(id)) }
      val combo = ctx.run(filtered)
      inside(combo.info.ast) {
        // Although the original AST is Filter(Filter(...), ...) the query compiler combines them into a single Filter
        case Filter(
              Entity("Person", List(), _),
              _,
              BinaryOperation(
                  BinaryOperation(_, _, tagA: ScalarTag),
                  _,
                  BinaryOperation(_, _, tagB: ScalarTag)
              )
            ) =>
              if (tagA.uid != tagB.uid) succeed
              else fail(s"The tags of the two entity clauses were the same in: ${qprint(combo.info.ast)}")

      }
    }

    // Technically this is the same as the test above but the formulation of the issue is slightly different.
    // this test is taken from the issue: https://github.com/zio/zio-protoquill/issues/318
    "dynamic(Quoted[Query]).filter(p => DynamicList(p == condA || p == condB || ...))" in {
      inline def selectName(p: Quoted[Person]) = {
        val nameFilter = Seq("a", "b")
        val regions = nameFilter.map(name => quote(p.name == lift(name)))
        regions.reduce((l, r) => quote(l || r))
      }

      val query = dynamicQuery[Person].filter(p => selectName(p))

      // Just to show that output from https://github.com/zio/zio-protoquill/issues/318 is good
      ctx.translate(query) mustBe "SELECT v0.id, v0.name, v0.age, v0.sex FROM Person v0 WHERE v0.name = 'a' OR v0.name = 'b'"
      val combo = ctx.run(query)

      inside(combo.info.ast) {
        // Although the original AST is Filter(Filter(...), ...) the query compiler combines them into a single Filter
        case Filter(
              Entity("Person", List(), _),
              _,
              BinaryOperation(
                  BinaryOperation(_, _, tagA: ScalarTag),
                  BooleanOperator.||,
                  BinaryOperation(_, _, tagB: ScalarTag)
              )
            ) =>
              if (tagA.uid != tagB.uid) succeed
              else fail(s"The tags of the two entity clauses were the same in: ${qprint(combo.info.ast)}")

      }
    }

    // Testing the batch variation of UID dupes. This example was provided by ex0ns here: https://github.com/zio/zio-protoquill/pull/398#discussion_r1428849465
    "dynamic batch" in {
      val nameFilter = Seq("John", "Doe")

      val people = List(Person(1, "Joe", 123, Sex.Male), Person(2, "Jill", 456, Sex.Female))

      val updateDynamic: Quoted[Person => Update[Person]] = quote {
        (p: Person) => query[Person].filter(p =>
            nameFilter.map(n => quote(sql"${p.name} == ${lift(n)}".asCondition)).reduce(_ || _)
          ).updateValue(p)
      }
      val mirror: BatchActionMirror = ctx.run { liftQuery(people).foreach(p => updateDynamic(p)) }
      ctx.translate { liftQuery(people).foreach(p => updateDynamic(p)) } mustBe
        List(
          "UPDATE Person AS p SET id = p.id, name = p.name, age = p.age, sex = p.sex FROM (VALUES (1, 'Joe', 123, 'male')) AS p(id, name, age, sex) WHERE p.name == 'John' OR p.name == 'Doe'",
          "UPDATE Person AS p SET id = p.id, name = p.name, age = p.age, sex = p.sex FROM (VALUES (2, 'Jill', 456, 'female')) AS p(id, name, age, sex) WHERE p.name == 'John' OR p.name == 'Doe'"
        )

      println(qprint(mirror.info.ast))

      inside(mirror.info.ast) {
        case Update(
              Filter(
                Entity("Person", List(), _),
                _,
                BinaryOperation(
                  Infix(_, List(_, nameA: ScalarTag), _, _, _),
                  BooleanOperator.||,
                  Infix(_, List(_, nameB: ScalarTag), _, _, _)
                )
              ),
              _
            ) =>
              if (nameA.uid != nameB.uid) succeed
              else fail(s"The tags of the two entity clauses were the same in: ${qprint(mirror.info.ast)}")
      }
    }
  }

}