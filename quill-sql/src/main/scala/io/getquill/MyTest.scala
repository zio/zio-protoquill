package io.getquill

import io.getquill.util.debug.PrintMac

object MyTest {
  import Forwarders.{given, *}

  val ctx = new SqlMirrorContext(PostgresDialect, Literal)
  import ctx._
  case class Person(name: String, age: Int)

  // inline def filteredPeople: Quoted[(Person => Boolean) => EntityQuery[Person]] = quote {
  //   (p: Person => Boolean) => query[Person].filter(p)
  // }
  // inline def joesFilter: Quoted[Person => Boolean] = quote {
  //   (p: Person) => p.name == "Joe"
  // }
  // inline def f = quote {
  //   filteredPeople(joesFilter)
  // }

  // inline def people: Quoted[Query[Person]] = quote { query[Person] }
  // inline def filterJoes: Quoted[Person => Boolean] = quote { (p: Person) => p.name == "Joe" }

  // // val joes = quote { people.filter(filterJoes)} // - Cannot parse. Bug!
  // inline def joes = quote { people.filter(p => p == "Joe") }
  // println(run(joes))

  // ================= ANOTHER EXAMPLE =====================
  // inline def person: Quoted[Person] = quote { query[Person].take(1).value.orNull }
  // inline def filterJoes: Quoted[Person => Boolean] = quote { (p: Person) => p.name == "Joe" }
  // inline def a = quote { filterJoes.apply(query[Person].take(1).value.orNull) } // Dynamic case for this doesn't work (fail with "cannot beta-reduce proxy error") need to look into it
  // inline def b = quote { filterJoes.apply(person) } // Dynamic case for this doesn't work (fail with "cannot beta-reduce proxy error") need to look into it
  // inline def c = quote { ((p: Person) => p.name == "Joe")(query[Person].take(1).value.orNull) }
  // inline def d = quote { ((p: Person) => p.name == "Joe").apply(person) }
  // println(run(a))
  // println(run(b))
  // println(run(c))
  // println(run(d))

  // println(run(q))
}

// Unquoteable[Quoted[T], T]

// class Iterable[+A]:
//   def concat[F[_]](xs: F[A])(using Foldable[F]): Iterable[A] = ...
// implicit Conversion[SomeCollection[T], Foldable[T]]
// class Iterable[+A]:
//   def concat(xs: ~Foldable[A]): Iterable[A] = ...

// object MyTest {
//   import io.getquill._
//   val ctx = new SqlMirrorContext(PostgresDialect, Literal)
//   import ctx._
//   case class Person(name: String, age: Int)

//   def main(args: Array[String]): Unit = {
//     val v = "DYNAMIC_FUNCTION"
//     inline def q = quote {
//       query[Person].map(p => infix"#$v(${p.name})".as[String])
//     }
//     println(run(q).string)
//   }

//   // query[Person].map(p => infix"func(${p.name}, #$v, ${lift(v)})".as[String])
//   // UPDATE Person AS p SET birthYear = ? WHERE p.id = ? RETURNING computedAge

//   // case class system(id: Int, version: Int)

//   // sealed trait VersionFilter
//   // object VersionFilter {
//   //   case class Specific(v: Int) extends VersionFilter //
//   //   case object TotalMax extends VersionFilter
//   // }

//   // inline def systemByVersion(inline filter: VersionFilter) =
//   //   inline filter match
//   //     case VersionFilter.Specific(v) =>
//   //       query[system].filter(s => s.version == v)
//   //     case VersionFilter.TotalMax =>
//   //       query[system].filter(s =>
//   //         s.version ===
//   //           query[system].filter(s2 => s2.id == s.id).map(_.version).max
//   //       )

//   // def fun1(): Unit = {
//   //   run(systemByVersion(VersionFilter.Specific(123)))

//   //   run(systemByVersion(VersionFilter.TotalMax))

//   // }
// }

// // import io.getquill.parser.BooSerializer
// // import io.getquill.quat.Quat
// // import io.getquill.util.debug.PrintMac
// // import io.getquill.parser.DoSerialize
// // import io.getquill.parser.SerializationBehavior
// // import io.getquill.context.mirror.Row

// // object MyTest:
// //   import io.getquill._
// //   val ctx = new SqlMirrorContext(PostgresDialect, Literal)
// //   import ctx._

// //   case class Person(name: String, age: Int)

// //   implicit class LimitQuery[T](q: Query[T]) {
// //     def limitQuery = quote(infix"$q LIMIT 1".as[Query[T]])
// //   }
// //   inline def q = quote { query[Person].limitQuery }
// //   def main(args: Array[String]): Unit = {
// //     println(run(q))
// //   }

// // // given DoSerialize with
// // //   override type BehaviorType = SerializationBehavior.SkipSerialize

// // // case class Name(first: String, last: String)
// // // case class Person(id: Int, name: Option[Name], age: Int)
// // // case class Contact(id: Int, person: Option[Person], num: Int)

// // // def main(args: Array[String]): Unit = { // ////
// // //   PrintMac(run(query[Contact]))

// // //   println(run(query[Contact]).extractor(Row.fromList(1, 2, "Joe", "Bloggs", 123, 456), MirrorSession.default))
// // //   println(run(query[Contact]).extractor(Row.fromList(1, 2, null, null, 123, 456), MirrorSession.default))
// // //   println(run(query[Contact]).extractor(Row.fromList(1, null, null, null, null, 456), MirrorSession.default))
// // //   println(run(query[Contact]).extractor(Row.fromList(1, null, null, null, null, null), MirrorSession.default))
// // //   println(run(query[Contact]).extractor(Row.fromList(null, null, null, null, null, null), MirrorSession.default))
// // // }

// // // val n = "Joe"
// // // val pp = Person("JoeJoe", 123)
// // // inline def q = quote {
// // //   query[Person].filter(p => p.name == lift("Joe")).updateValue(lift(pp))
// // // }

// // // val v = "baz" *: ("foo", "bar")

// // // // ctx.run {
// // // //   query[Person].map(p => (p.id, p.name) ++ (p.name, p.id))
// // // // }

// // // PrintMac(
// // //   ("foo", 1) ++ (2, "bar")
// // // )

// // //PrintMac(q)
// // //println(q.lifts)
// // //println(q)
// // //println( run(q).string ) ////////////////

// // // also, in InsertUpdateMacro, // TODO Need a catch-all

// // // TODO Need a static test, a dynamic test, and one of each where it's not 'lift'
