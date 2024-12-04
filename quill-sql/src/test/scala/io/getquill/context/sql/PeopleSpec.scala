package io.getquill.context.sql

import io.getquill.context.Context
import io.getquill.*
import io.getquill.generic.GenericEncoder
import io.getquill.generic.{DecodingType, GenericDecoder}

trait PeopleSpec extends Spec { self =>
  val context: Context[_, _] {
    type Session = self.Session
    type PrepareRow = self.PrepareRow
    type ResultRow = self.ResultRow
  }
  import context._

  case class Person(name: String, age: Int)
  case class Couple(her: String, him: String)
  given personDecoder: GenericDecoder[ResultRow, Session, Person, DecodingType.Composite] = deriveComposite
  given coupleDecoder: GenericDecoder[ResultRow, Session, Couple, DecodingType.Composite] = deriveComposite

  inline def peopleInsert =
    quote((p: Person) => query[Person].insertValue(p))

  val peopleEntries = List(
    Person("Alex", 60),
    Person("Bert", 55),
    Person("Cora", 33),
    Person("Drew", 31),
    Person("Edna", 21),
    Person("Fred", 60)
  )

  inline def couplesInsert =
    quote((c: Couple) => query[Couple].insertValue(c))

  val couplesEntries = List(
    Couple("Alex", "Bert"),
    Couple("Cora", "Drew"),
    Couple("Edna", "Fred")
  )

  inline def `Ex 0.1 simple` =
    quote {
      query[Person]
    }

  val `Ex 0.1 expected result` = peopleEntries

  inline def `Ex 0.2 simple mapped` =
    quote {
      query[Person].map(p => (p.name, Option(p.age)))
    }

  val `Ex 0.2 expected result` = peopleEntries.map(p => (p.name, Some(p.age)))

  inline def `Ex 1 differences` =
    quote {
      for {
        c <- query[Couple]
        w <- query[Person]
        m <- query[Person] if (c.her == w.name && c.him == m.name && w.age > m.age)
      } yield {
        (w.name, w.age - m.age)
      }
    }
  val `Ex 1 expected result` = List(("Alex", 5), ("Cora", 2))

  inline def `Ex 2 rangeSimple` = quote {
    (a: Int, b: Int) =>
      for {
        u <- query[Person] if (a <= u.age && u.age < b)
      } yield {
        u
      }
  }
  val `Ex 2 param 1` = 30
  val `Ex 2 param 2` = 40
  val `Ex 2 expected result` = List(Person("Cora", 33), Person("Drew", 31))

  inline def satisfies =
    quote {
      (p: Int => Boolean) =>
        for {
          u <- query[Person] if (p(u.age))
        } yield {
          u
        }
    }
  inline def `Ex 3 satisfies` = quote(satisfies((x: Int) => 20 <= x && x < 30))
  val `Ex 3 expected result` = List(Person("Edna", 21))

  inline def `Ex 4 satisfies` = quote(satisfies((x: Int) => x % 2 == 0))
  val `Ex 4 expected result` = List(Person("Alex", 60), Person("Fred", 60))

  // TODO this one has to be dynamic because you can't have nested inlines
  // should look into how to make a static equivalent
  val `Ex 5 compose` = {
    val range = quote {
      (a: Int, b: Int) =>
        for {
          u <- query[Person] if (a <= u.age && u.age < b)
        } yield {
          u
        }
    }
    val ageFromName = quote {
      (s: String) =>
        for {
          u <- query[Person] if (s == u.name)
        } yield {
          u.age
        }
    }
    quote {
      (s: String, t: String) =>
        for {
          a <- ageFromName(s)
          b <- ageFromName(t)
          r <- range(a, b)
        } yield {
          r
        }
    }
  }
  val `Ex 5 param 1` = "Drew"
  val `Ex 5 param 2` = "Bert"
  val `Ex 5 expected result` = List(Person("Cora", 33), Person("Drew", 31))

  sealed trait Predicate
  case class Above(i: Int) extends Predicate
  case class Below(i: Int) extends Predicate
  case class And(a: Predicate, b: Predicate) extends Predicate
  case class Or(a: Predicate, b: Predicate) extends Predicate
  case class Not(p: Predicate) extends Predicate

  // TODO Leaving this dynamic for now. Should look into a static variant later
  def eval(t: Predicate): Quoted[Int => Boolean] =
    t match {
      case Above(n)    => quote((x: Int) => x > lift(n))
      case Below(n)    => quote((x: Int) => x < lift(n))
      case And(t1, t2) => quote((x: Int) => eval(t1)(x) && eval(t2)(x))
      case Or(t1, t2)  => quote((x: Int) => eval(t1)(x) || eval(t2)(x))
      case Not(t0)     => quote((x: Int) => !eval(t0)(x))
    }

  val `Ex 6 predicate` = And(Above(30), Below(40))
  val `Ex 6 expected result` = List(Person("Cora", 33), Person("Drew", 31))

  val `Ex 7 predicate` = Not(Or(Below(20), Above(30)))
  val `Ex 7 expected result` = List(Person("Edna", 21))

  inline def `Ex 8 and 9 contains` =
    quote {
      (set: Query[Int]) =>
        query[Person].filter(p => set.contains(p.age))
    }

  val `Ex 8 param` = Set.empty[Int]
  val `Ex 8 expected result` = List.empty[Person]

  val `Ex 9 param` = Set(55, 33)
  val `Ex 9 expected result` = List(Person("Bert", 55), Person("Cora", 33))

  inline def `Ex 10 page 1 query` = quote {
    query[Person].sortBy(p => p.name)(Ord.asc).drop(0).take(3)
  }
  val `Ex 10 page 1 expected` = peopleEntries.sortBy(_.name).slice(0, 3)
  inline def `Ex 10 page 2 query` = quote {
    query[Person].sortBy(p => p.name)(Ord.asc).drop(3).take(3)
  }
  val `Ex 10 page 2 expected` = peopleEntries.sortBy(_.name).slice(3, 6)
}
