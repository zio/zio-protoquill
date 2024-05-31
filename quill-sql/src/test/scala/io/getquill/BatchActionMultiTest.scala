package io.getquill

import io.getquill.ast.*
import io.getquill.context.{Context, ExecutionType}
import io.getquill.context.ExecutionType.{Dynamic, Static}
import io.getquill.quat.quatOf
import io.getquill.{QuotationLot, QuotationVase, Quoted, query, quote}
import io.getquill.util.debug.PrintMac
import org.scalatest.*

import scala.language.implicitConversions

class BatchActionMultiTest extends Spec with Inside with SuperContext[PostgresDialect, Literal] {
  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)

  import ctx.*

  "Multi-row Batch Action Should work with" - {
    "inserts > batch-size - (2rows + 2rows) + (1row)" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
        Person(5, "E", 555, Sex.Female)
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
            List(List(1, "A", 111, "male", 2, "B", 222, "male"), List(3, "C", 333, "male", 4, "D", 444, "female")),
            executionType
          ),
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)",
            List(List(5, "E", 555, "female")),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }
      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }

      def expect2(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ((? || ?) || 'bar'), ?), (?, ((? || ?) || 'bar'), ?, ?)",
            List(List(1, "foo", "A", 111, "male", 2, "foo", "B", 222, "male"), List(3, "foo", "C", 333, "male", 4, "foo", "D", 444, "female")),
            executionType
          ),
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ((? || ?) || 'bar'), ?, ?)",
            List(List(5, "foo", "E", 555, "female")),
            executionType
          )
        )

      "static - mixed" in {
        val static = ctx.run(liftQuery(people).foreach(p => query[Person].insert(_.id -> p.id, _.name -> (lift("foo") + p.name + "bar"), _.age -> p.age, _.sex -> p.sex)), 2)
        static.tripleBatchMulti mustEqual expect2(ExecutionType.Static)
      }
      "dynamic - mixed" in {
        // TODO Why does it not print that a dynamic query is being run?
        val q = quote(liftQuery(people).foreach(p => query[Person].insert(_.id -> p.id, _.name -> (lift("foo") + p.name + "bar"), _.age -> p.age, _.sex -> p.sex)))
        val static = ctx.run(q, 2)
        static.tripleBatchMulti mustEqual expect2(ExecutionType.Dynamic)
      }
    }

    "batch insert - (2rows + 2rows)" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
            List(List(1, "A", 111, "male", 2, "B", 222, "male"), List(3, "C", 333, "male", 4, "D", 444, "female")),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }

      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }
    }

    "inserts == batch-size" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
            List(List(1, "A", 111, "male", 2, "B", 222, "male")),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }

      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }
    }

    "inserts < batch-size - (1row)" - {
      val people = List(Person(1, "A", 111, Sex.Male))

      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)",
            List(List(1, "A", 111, "male")),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }

      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }
    }

    "fallback for non-insert query (in a context that doesn't support update)" - {
      val ctx: MirrorContext[MySQLDialect, Literal] = new MirrorContext[MySQLDialect, Literal](MySQLDialect, Literal)
      import ctx.*
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
        Person(5, "E", 555, Sex.Female)
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "UPDATE Person pt SET id = ?, name = ?, age = ?, sex = ? WHERE pt.id = ?",
            List(
              List(1, "A", 111, "male", 1),
              List(2, "B", 222, "male", 2),
              List(3, "C", 333, "male", 3),
              List(4, "D", 444, "female", 4),
              List(5, "E", 555, "female", 5)
            ),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => updatePeopleById(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }

      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => updatePeopleByIdDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }
    }

    "update query" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
        Person(5, "E", 555, Sex.Female)
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "UPDATE Person AS pt SET id = p.id1, name = p.name, age = p.age, sex = p.sex FROM (VALUES (?, ?, ?, ?, ?), (?, ?, ?, ?, ?)) AS p(id, id1, name, age, sex) WHERE pt.id = p.id",
            List(List(1, 1, "A", 111, "male", 2, 2, "B", 222, "male"), List(3, 3, "C", 333, "male", 4, 4, "D", 444, "female")),
            executionType
          ),
          (
            "UPDATE Person AS pt SET id = p.id1, name = p.name, age = p.age, sex = p.sex FROM (VALUES (?, ?, ?, ?, ?)) AS p(id, id1, name, age, sex) WHERE pt.id = p.id",
            List(List(5, 5, "E", 555, "female")),
            executionType
          )
        )

      "static" in {
        val static = ctx.run(liftQuery(people).foreach(p => updatePeopleById(p)), 2)
        static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }

      "dynamic" in {
        val dynamic = ctx.run(liftQuery(people).foreach(p => updatePeopleByIdDynamic(p)), 2)
        dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
      }
    }

    "supported contexts" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
        Person(5, "E", 555, Sex.Female)
      )

      def makeRow(executionType: ExecutionType)(queryA: String, queryB: String) =
        List(
          (
            queryA,
            List(List(1, "A", 111, "male", 2, "B", 222, "male"), List(3, "C", 333, "male", 4, "D", 444, "female")),
            executionType
          ),
          (
            queryB,
            List(List(5, "E", 555, "female")),
            executionType
          )
        )

      def expect(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
          "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)"
        )

      def expectH2(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age,sex) VALUES ($1, $2, $3, $4), ($5, $6, $7, $8)",
          "INSERT INTO Person (id,name,age,sex) VALUES ($1, $2, $3, $4)"
        )

      def expectPostgresReturning(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?), (?, ?, ?, ?) RETURNING id",
          "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?) RETURNING id"
        )

      def expectSqlServerReturning(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age,sex) OUTPUT INSERTED.id VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
          "INSERT INTO Person (id,name,age,sex) OUTPUT INSERTED.id VALUES (?, ?, ?, ?)"
        )

      "postgres - regular/returning" in {
        val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext(PostgresDialect, Literal)
        import ctx.*
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectPostgresReturning(ExecutionType.Static)
      }
      "sqlserver - regular/returning" in {
        val ctx: MirrorContext[SQLServerDialect, Literal] = new MirrorContext(SQLServerDialect, Literal)
        import ctx.*
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectSqlServerReturning(ExecutionType.Static)
      }
      "mysql - regular/returning" in {
        val ctx: MirrorContext[MySQLDialect, Literal] = new MirrorContext(MySQLDialect, Literal)
        import ctx.*
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }
      "h2 - regular/returning" in {
        val ctx: MirrorContext[H2Dialect, Literal] = new MirrorContext(H2Dialect, Literal)
        import ctx.*
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expectH2(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectH2(ExecutionType.Static)
      }
      "sqlite - only regular" in {
        val ctx: MirrorContext[SqliteDialect, Literal] = new MirrorContext(SqliteDialect, Literal)
        import ctx.*
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }
    }

    "fallback for non-supported context" - {
      val people = List(
        Person(1, "A", 111, Sex.Male),
        Person(2, "B", 222, Sex.Male),
        Person(3, "C", 333, Sex.Male),
        Person(4, "D", 444, Sex.Female),
        Person(5, "E", 555, Sex.Female)
      )

      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age,sex) VALUES (?, ?, ?, ?)",
            List(
              List(1, "A", 111, "male"),
              List(2, "B", 222, "male"),
              List(3, "C", 333, "male"),
              List(4, "D", 444, "female"),
              List(5, "E", 555, "female")
            ),
            executionType
          )
        )

      "oracle" - {
        val ctx: MirrorContext[OracleDialect, Literal] = new MirrorContext[OracleDialect, Literal](OracleDialect, Literal)
        import ctx.*
        "static" in {
          val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2)
          static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
        }
        "dynamic" in {
          val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p)), 2)
          dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
        }
      }
      "sqlite - with returning clause" - {
        val ctx: MirrorContext[OracleDialect, Literal] = new MirrorContext[OracleDialect, Literal](OracleDialect, Literal)
        import ctx.*
        "static" in {
          val static = ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2)
          static.tripleBatchMulti mustEqual expect(ExecutionType.Static)
        }
        "dynamic" in {
          val dynamic = ctx.run(liftQuery(people).foreach(p => insertPeopleDynamic(p).returning(_.id)), 2)
          dynamic.tripleBatchMulti mustEqual expect(ExecutionType.Dynamic)
        }
      }
    }
  }
}
