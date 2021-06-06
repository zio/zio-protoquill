# Introduction

ProtoQuill is the continuation of [Quill: Free/Libre Compile-time Language Integrated Queries for Scala](https://getquill.io/). Althought is is already considerably feature-full, it is still very-much in beta so please use it carefuly. For those migrating, or exploring migration from Scala2-Quill, most Queries written in Scala2-Quill should work readily in ProtoQuill but they will become Dynamic. Change them to `inline def` expressions and they should once-again be compile-time (see the Rationale section for some info on why I chose to do this). Also see the Migration Notes section.

Not all Contexts and not all Functionality is Supported yet. Here is a rough list of both:

Currently Supported:
 - Basic Quotation, Querying, Lifting, and Composition (Compile-Time and Dynamic)
 - Inner/Outer, Left/Right joins
 - Query.map/flatMap/concatMap/filter other [query constructs](https://getquill.io/#quotation-queries).
 - Insert, Update, Delete (Actions)[https://getquill.io/#quotation-actions] (Compile-Time and Dynamic)
 - Batch Insert, Batch Update, and Batch Delete Actions (currently only Compile-Time)
 - ZIO, Synchronous JDBC, and Jasync Postgres contexts.

Currently Not Supported:
 - SQL OnConflict Clauses (Coming Soon!)
 - Prepare Query (i.e. `context.prepare(query)`)
 - Dynamic Query API (i.e. [this](https://getquill.io/#quotation-dynamic-queries-dynamic-query-api))
 - [Implicit Query](https://getquill.io/#quotation-implicit-query)
 - [IO Monad](https://getquill.io/#quotation-io-monad)
 - Cassandra Contexts (Coming Soon!)
 - Monix JDBC (and Cassandra) Contexts (Coming Soon!)
 - Lagom Contexts
 - OrientDB Contexts
 - Spark Context (still waiting on Spark for Scala 2.13)

One other note that this documentation is not yet a fully-fledged reference for ProtoQuill features. Have a look at the original [Quill documentation](https://getquill.io/) for basic information about how Quill constructs (e.g. Queries, Joins, Actions, Batch Actions, etc...) are written in lieu of any documentation missing here.

For further information, watch:
 - [Quill, Dotty, And The Awesome Power of 'Inline'](https://www.youtube.com/watch?v=SmBpGkIsJIU) - Many examples of new things that can be done with this library that cannot be done with standard Quill.
 - [ScQuilL Sessions - Quill, Dotty, and Macros](https://www.youtube.com/watch?v=0PSg__PPjY8&list=PLqky8QybCVQYNZY_MNJpkjFKT-dAdHQDX) - A tutorial on developing Dotty-Quill from scratch (covers quoting, liftables, and liftables).
 - [Generic Derivation is the New Reflection](https://www.youtube.com/watch?v=E9L1-rkYPng) - A tutorial on how Dotty Generic Derivation works covering a Dotty-Quill use-case.


# Getting Started

The simplest way to get started with ProtoQuill is with the standard JDBC contexts.
These are sychronous so for a high-throughput system you will ultimately need to switch
to either the ZIO-based contexts, Jasync, or the Monix ones (Monix contexts coming soon!)

Add the following to your SBT file:
```
```

Assuming we are using Postgres, add the following application.conf.
```
testPostgresDB.dataSourceClassName=org.postgresql.ds.PGSimpleDataSource
testPostgresDB.dataSource.databaseName=<my-database>
testPostgresDB.dataSource.url=<my-jdbc-url>
```
Have a look at [this list](https://getquill.io/#contexts-quill-jdbc) to see how to configure other databases.

Create a context and a case class representing your table (code generation is also available).
```scala
import io.getquill._

object MyApp {
  case class Person(firstName: String, lastName: String, age: Int)

  // SnakeCase turns firstName -> first_name
  val ctx = new PostgresJdbcContext(SnakeCase, "ctx")
  import ctx._

  def main(args: Array[String]): Unit = {
    val named = "Joe"
    inline def somePeople = quote { 
      query[Person].filter(p => p.firstName == lift(named))
    }
    val people: List[Person] = run(somePeople)
    // TODO Get SQL
    println(people)
  }
}
```

# Tutorial

## Queries

ProtoQuill queries are built using inline quoted expressions.
```scala
// With just this import you can use quote, query, insert/update/delete and lazyLift
import io.getquill._

inline def people = quote { 
  query[Person] 
}
inline def joes = quote {
  people.filter(p => p.name == "Joe")
}

> You *do not* need to import a context in ProtoQuill to make a quoation, just `io.getquill._`. Contexts are only needed for lifting. See the `Lifting and Lazy Lifting` section for more detail.

run(joes)
// TODO Get SQL
```

### Quotaion is (Mostly) Optional

If all parts of a Query are `inline def`, quotation is not strictly necessary:

```scala
inline def people = query[Person]
inline def joes = people.filter(p => p.name == "Joe")

run(joes)
// TODO Get SQL
```

However, if parts of the the query are dynamic (i.e. not `inline def`) it is needed:
```scala
inline def people = quote { 
  query[Person] 
}
val joes = quote {
  people.filter(p => p.name == "Joe")
}

run(joes)
// TODO Warning Dynamic Query
```

### Shareable Code

Since quotation of `inline def` code is optional, Quill expressions can share code with regular Scala constructs.

```scala
// case class Person(name: String, age: Int)
inline def onlyJoes(p: Person) = p.name == "Joe"

run( query[Person].filter(p => onlyJoes(p)) )
// TODO Get SQL

val people: List[Person] = ...
val joes = people.filter(p => onlyJoes(p))
```

### Advanced Example

Since Quill expressions can share code with regular Scala constructs,
this can be generalized into higher-level constructs such as typeclasses.

```scala
// case class Person(name: String, age: Int)

trait Filterable[F[_]]:
  extension [A](inline x: F[A])
    inline def filter(inline f: A => Boolean): F[A]

extension [F[_]](inline people: F[Person])(using inline filterable: Filterable[F])
  inline def onlyJoes = people.filter(p => p.name == "Joe")

class ListFilterable extends Filterable[List]:
  extension [A](inline xs: List[A])
    inline def filter(inline f: A => Boolean): List[A] = xs.filter(f)

class QueryFilterable extends Filterable[List]:
  extension [A](inline xs: List[A])
    inline def filter(inline f: A => Boolean): List[A] = xs.filter(f)

run( query[Person].joes )
// GET SQL

val people: List[Person] = ...
val joes = people.onlyJoes
```
Using this technique, a standard functor-hierarchy can be constructed that can be used for compile-time
generation of Quill Queries. Have a look at the [Typeclass-series examples](https://github.com/getquill/protoquill/tree/master/quill-sql-tests/src/test/scala/io/getquill/examples) for more inspiration.


## Lifting and Lazy Lifting

Since Quill-Quotations define blocks of compile-time-inspectable code, adding variables whose value is only know during runtime typically requires lifting:
```scala
// NOTE: Be sure to import a context first!
// val ctx = new MirrorSqlContext(PostgresDialect, Literal); import ctx._

val runtimeValue = somethingFromSomewhere() // A value that we only know during runtime
inline def somePeople = quote {
  query[Person].filter(p => p.name == lift(runtimeValue))
}
val results: List[Person] = run(somePeople)
// TODO Get SQL
```

However, since Quotation in ProtoQuill is static, you can use `lazyLift` to lift a value without importing a context. The advantage of this is that you can lift things before what context to use.

```scala
import io.getquill._
val name = ...
inline def q = quote { query[Person].filter(p => p.name == lift(name)) }

// Now we can use this quotation in multiple contexts
{
  val ctx = new PostgresJdbcContext(Literal, "ctx")
  val results = ctx.run(q)
  // TODO Get SQL
}
{
  val ctx = new H2JdbcContext(Literal, "ctx")
  val results = ctx.run(q)
  // TODO Get SQL
}
```
Note however that for lazy-lifts to work, for some query, all of it's parts need to be `inline def`. That is to say, Dynamic Queries do not work with `lazyLift`.


### How it Works

Internally, the lift-method will use the mechanisms of the underlying database-layer (e.g. JDBC) to lift the `runtimeValue` ultimately swapping it into the `"?"` location using some kind of prepared statement. This is typically handled by the encoders.

```scala
/* Note that is an approximate example! Not actual Quill code. */

trait Encoders:
  implicit val stringEncoder: Encoder[String] =
    JdbcEncoder(sqlType, (index: Int, value: String, row: PrepareRow) => {
      row.setString(index, value)
      row
    })

class MyDatabaseContext(...) with Encoders

def lift(value: T)(using Encoder[T]) = ...
```
> Note that the above is only a conceptual model of how `lift` works. In reality it needs to be implemented using Scala 3 Macros in order to function properly.

This means that in order to do lifting we must first import a context.
```scala
// Must do this:
val ctx = new MyDatabaseContext()
import ctx._

// Before Doing this:
inline def somePeople = quote {
  query[Person].filter(p => p.name == lift(runtimeValue))
}
```


### Filtering table fields with a Map
On typical use-case that ProtoQuill can do (which has been difficult in the past) is to filter a query based on an arbitrary group of column/value pairs. This is typically done with Http-Based systems where URL-parameters `&key=value` are decoded as a map. In ProtoQuill, the `filterByKeys` addresses this use-case.
```scala
val values: Map[String, String] = Map("firstName" -> "Joe", "age" -> "22")

// filterByKeys uses lift so you need a context to use it
val ctx = new MirrorContext(Literal, PostgresDialect)
import ctx._

inline def q = quote {
  query[Person].filterByKeys(values)
}
run(q)

// SELECT p.firstName, p.lastName, p.age 
// FROM Person p 
// WHERE
//   (p.firstName = ? OR ? IS NULL) AND
//   (p.lastName = ? OR ? IS NULL) AND
//   (p.age = ? OR ? IS NULL) AND
//   true
```

### FilterOne

TBD


# Migration Notes

TBD
 - Changing to inline def
 - Quotation is now static, impact on imports (i.e. context now always needed)
 - Issues with batch queries (since compile-time only for now)

# Extensions

ProtoQuill supports standard Dotty extensions. An inline extension will yield a compile-time query.

```
case class Person(first: String, last: String)

extension (inline p: Person) // make sure this variable is `inline`
  inline def fullName = first + " " + last

run( query[Person].map(p => p.fullName) )
// TODO Get SQL
```

# Rationale for Inline

For a basic reasoning of why Inline was chosen (instead of Refined-Types on `val` expressions) have a look at the video: [Quill, Dotty, And The Awesome Power of 'Inline'](https://github.com/getquill/protoquill). A more thorough explination is TBD.


### Interesting Ideas
 - Implement a `lazyFilterByKeys` method using the same logic as filterByKeys but using lazy lifts.
   in order to be able to use this kind of functionality with out having to import a context.
 - Implement a `filterByLikes` which is the same as `filterByKeys` but uses `like` instead of `==`.
   then can also implement a `lazyFilterByLikes`.
 - Write a `query.filter(p => p.firstName.inSet("foo", lift(bar), "baz"))` using the `ListFlicer`.
   this could either translate into `WHERE firstName == 'foo' OR firstName == ? OR firstName == 'baz'` or 
   `WHERE firstName in ('foo', ?, 'baz')`.
 - Combine MapFlicer and ListFilcer to allow up to N maps to filter each field up to N times
   this would be very useful with `like` in order to check that a field matches multiple patterns
   e.g. `... FROM Person p WHERE p.firstName like 'j%' AND p.firstName like '%e' i.e. find
   all people whose name starts with 'j' and ends with 'e'.

