package io.getquill

import scala.language.implicitConversions
import io.getquill.Quoted
import io.getquill.ast._
import io.getquill.QuotationLot
import io.getquill.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

import io.getquill.context.ExecutionType.Static
import io.getquill.context.ExecutionType.Dynamic
import io.getquill.context.Context
import io.getquill.quote
import io.getquill.query
import io.getquill.util.debug.PrintMac

class BatchActionMultiTest extends Spec with Inside with SuperContext[PostgresDialect, Literal] {
  // Need to fully type this otherwise scala compiler thinks it's still just 'Context' from the super-class
  // and the extensions (m: MirrorContext[_, _]#BatchActionMirror) etc... classes in Spec don't match their types correctly
  val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext[PostgresDialect, Literal](PostgresDialect, Literal)
  import ctx._

  "Multi-row Batch Action Should work with" - {
    "inserts > batch-size - (2rows + 2rows) + (1row)" - {
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444), Person(5, "E", 555))
      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?), (?, ?, ?)",
            List(List(1, "A", 111, 2, "B", 222), List(3, "C", 333, 4, "D", 444)),
            executionType
          ),
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?)",
            List(List(5, "E", 555)),
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
            "INSERT INTO Person (id,name,age) VALUES (?, ((? || ?) || 'bar'), ?), (?, ((? || ?) || 'bar'), ?)",
            List(List(1, "foo", "A", 111, 2, "foo", "B", 222), List(3, "foo", "C", 333, 4, "foo", "D", 444)),
            executionType
          ),
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ((? || ?) || 'bar'), ?)",
            List(List(5, "foo", "E", 555)),
            executionType
          )
        )
      "static - mixed" in {
        val static = ctx.run(liftQuery(people).foreach(p => query[Person].insert(_.id -> p.id, _.name -> (lift("foo") + p.name + "bar"), _.age -> p.age)), 2)
        static.tripleBatchMulti mustEqual expect2(ExecutionType.Static)
      }
      "dynamic - mixed" in {
        // TODO Why does it not print that a dynamic query is being run?
        val q = quote(liftQuery(people).foreach(p => query[Person].insert(_.id -> p.id, _.name -> (lift("foo") + p.name + "bar"), _.age -> p.age)))
        val static = ctx.run(q, 2)
        static.tripleBatchMulti mustEqual expect2(ExecutionType.Dynamic)
      }
    }

    "batch insert - (2rows + 2rows)" - {
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444))
      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?), (?, ?, ?)",
            List(List(1, "A", 111, 2, "B", 222), List(3, "C", 333, 4, "D", 444)),
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
      val people = List(Person(1, "A", 111), Person(2, "B", 222))
      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?), (?, ?, ?)",
            List(List(1, "A", 111, 2, "B", 222)),
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
      val people = List(Person(1, "A", 111))
      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?)",
            List(List(1, "A", 111)),
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
      import ctx._
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444), Person(5, "E", 555))
      def expect(executionType: ExecutionType) =
        List(
          (
            "UPDATE Person pt SET id = ?, name = ?, age = ? WHERE pt.id = ?",
            List(List(1, "A", 111, 1), List(2, "B", 222, 2), List(3, "C", 333, 3), List(4, "D", 444, 4), List(5, "E", 555, 5)),
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
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444), Person(5, "E", 555))
      def expect(executionType: ExecutionType) =
        List(
          (
            "UPDATE Person AS pt SET id = p.id1, name = p.name, age = p.age FROM (VALUES (?, ?, ?, ?), (?, ?, ?, ?)) AS p(id, id1, name, age) WHERE pt.id = p.id",
            List(List(1, 1, "A", 111, 2, 2, "B", 222), List(3, 3, "C", 333, 4, 4, "D", 444)),
            executionType
          ),
          (
            "UPDATE Person AS pt SET id = p.id1, name = p.name, age = p.age FROM (VALUES (?, ?, ?, ?)) AS p(id, id1, name, age) WHERE pt.id = p.id",
            List(List(5, 5, "E", 555)),
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
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444), Person(5, "E", 555))
      def makeRow(executionType: ExecutionType)(queryA: String, queryB: String) =
        List(
          (
            queryA,
            List(List(1, "A", 111, 2, "B", 222), List(3, "C", 333, 4, "D", 444)),
            executionType
          ),
          (
            queryB,
            List(List(5, "E", 555)),
            executionType
          )
        )

      def expect(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age) VALUES (?, ?, ?), (?, ?, ?)",
          "INSERT INTO Person (id,name,age) VALUES (?, ?, ?)"
        )

      def expectH2(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age) VALUES ($1, $2, $3), ($4, $5, $6)",
          "INSERT INTO Person (id,name,age) VALUES ($1, $2, $3)"
        )

      def expectPostgresReturning(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age) VALUES (?, ?, ?), (?, ?, ?) RETURNING id",
          "INSERT INTO Person (id,name,age) VALUES (?, ?, ?) RETURNING id"
        )

      def expectSqlServerReturning(executionType: ExecutionType) =
        makeRow(executionType)(
          "INSERT INTO Person (id,name,age) OUTPUT INSERTED.id VALUES (?, ?, ?), (?, ?, ?)",
          "INSERT INTO Person (id,name,age) OUTPUT INSERTED.id VALUES (?, ?, ?)"
        )

      "postgres - regular/returning" in {
        val ctx: MirrorContext[PostgresDialect, Literal] = new MirrorContext(PostgresDialect, Literal)
        import ctx._
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectPostgresReturning(ExecutionType.Static)
      }
      "sqlserver - regular/returning" in {
        val ctx: MirrorContext[SQLServerDialect, Literal] = new MirrorContext(SQLServerDialect, Literal)
        import ctx._
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectSqlServerReturning(ExecutionType.Static)
      }
      "mysql - regular/returning" in {
        val ctx: MirrorContext[MySQLDialect, Literal] = new MirrorContext(MySQLDialect, Literal)
        import ctx._
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }
      "h2 - regular/returning" in {
        val ctx: MirrorContext[H2Dialect, Literal] = new MirrorContext(H2Dialect, Literal)
        import ctx._
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expectH2(ExecutionType.Static)
        ctx.run(liftQuery(people).foreach(p => insertPeople(p).returning(_.id)), 2).tripleBatchMulti mustEqual expectH2(ExecutionType.Static)
      }
      "sqlite - only regular" in {
        val ctx: MirrorContext[SqliteDialect, Literal] = new MirrorContext(SqliteDialect, Literal)
        import ctx._
        ctx.run(liftQuery(people).foreach(p => insertPeople(p)), 2).tripleBatchMulti mustEqual expect(ExecutionType.Static)
      }
    }

    "fallback for non-supported context" - {
      val people = List(Person(1, "A", 111), Person(2, "B", 222), Person(3, "C", 333), Person(4, "D", 444), Person(5, "E", 555))
      def expect(executionType: ExecutionType) =
        List(
          (
            "INSERT INTO Person (id,name,age) VALUES (?, ?, ?)",
            List(List(1, "A", 111), List(2, "B", 222), List(3, "C", 333), List(4, "D", 444), List(5, "E", 555)),
            executionType
          )
        )

      "oracle" - {
        val ctx: MirrorContext[OracleDialect, Literal] = new MirrorContext[OracleDialect, Literal](OracleDialect, Literal)
        import ctx._
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
        import ctx._
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
