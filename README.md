## Future Dotty-Based Quill Implementation

Compile using sbt `compile` and `test:compile` normally. Currently on M3.

This project should work with metals. To import, go to the
project directory and type `code .` to open VS Code
(note that it will have to be configured beforehand).

The functionality that is currently available is:
 - Equivalent to quill-sql
 - query, insert, delete, update basic functionality
 - basic lift functionality `lift(scalar)` and `insert(lift(caseClass))`.
 - basic parsing of quoted clauses, see `Parser.scala` for details

### Getting started
In Proto-Quill you can get started with just the following code:
```scala
// With just this import you can use quote, query, insert/update/delete and lazyLift
import io.getquill.lib._

// Use inline-def to compose queries during compile time. Non inline-def queries
// will be dynamically evaluated.
inline def q = quote {
  query[Person].filter(p.firstName == "Joe")
}

// import a context to be able to run queries and lift runtime values
val l = "Bloggs"
inline def q2 = quote {
  q.filter(p => p.lastName == lift(l))
}

println(run(l).string)
```

### Interesting features:

#### Lazy Lifts
Using lazy lifts, you can lift runtime values without having to import a context.
Note that lazy lifts can only be used with compile-time queries.
```scala
import io.getquill.lib._
val n = "Joe"
val q = quote {
  query[Person].filter(p => p.firstName == lift(n))
}
```

#### Composing with Inline

You can use inline methods to compose functionality in static queries the way you would
typically be only be able to do things with dynamic queries.
```scala
inline def joes(q: Query[Person]): Query[Person] =
  q.filter(p => p.firstName == "Joe")

val ctx = new MirrorContext(Literal, PostgresDialect)
import ctx._

inline def q = quote {
  joes(query[Person])
}
run(q)
```
There are many, many things you can do with this functionality:
 - Compose queries using Typeclasses
 - Apply type-level logic with query compositions
 - Share code with Quill-Query operations with other Scala collections.

Have a look at the test/scala/io/getquill/examples package for more details
and watch the below video "Quill, Dotty, And The Awesome Power of 'Inline'"
for more information.

#### Filtering table fields with a Map
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

### Major Features Not Yet Implemented
 - liftQuery and Batch functionality
 - query-single (i.e. `run(scalar)`)
 - Shareable testing (i.e. with Quill 3)
 - JDBC, NDBC, Cassandra and all other effect-based contexts
  - Hopefully it will be possible to share this code from Quill 3 in the future.
 - Build and release functionality

### Media

For further information, watch:
 - [ScQuilL Sessions - Quill, Dotty, and Macros](https://www.youtube.com/watch?v=0PSg__PPjY8&list=PLqky8QybCVQYNZY_MNJpkjFKT-dAdHQDX) - A tutorial on developing Dotty-Quill from scratch (covers quoting, liftables, and liftables).
 - [Quill, Dotty, And The Awesome Power of 'Inline'](https://www.youtube.com/watch?v=SmBpGkIsJIU) - Many examples of new things that can be done with this library that cannot be done with standard Quill.
