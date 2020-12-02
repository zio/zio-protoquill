package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object TypeclassExample_For {
  import io.getquill._
  case class Address(fk: Int, street: String, zip: Int) extends Embedded
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._


  case class Person(id: Int, name: String, age: Int)

  trait Functor[F[_]]:
    extension [A, B](inline x: F[A]):
      inline def map(inline f: A => B): F[B]

  trait Monad[F[_]] extends Functor[F]:
    //inline def pure[A](x: A): F[A]
    extension [A, B](inline x: F[A]):
      inline def map(inline f: A => B): F[B]
      inline def flatMap(inline f: A => F[B]): F[B]
      
  trait For[F[_]]: //extends Monad[F]:
    extension [A, B](inline x: F[A]):
      inline def map(inline f: A => B): F[B]
      inline def flatMap(inline f: A => F[B]): F[B]
      inline def withFilter(inline f: A => Boolean): F[A]

  class ListFunctor extends Functor[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)

  class ListMonad extends Monad[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)
      inline def flatMap(inline f: A => List[B]): List[B] = xs.flatMap(f)

  class ListFor extends For[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)
      inline def flatMap(inline f: A => List[B]): List[B] = xs.flatMap(f)
      inline def withFilter(inline f: A => Boolean): List[A] = xs.filter(f)
   
  class QueryFunctor extends Functor[Query]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)

  class QueryMonad extends Monad[Query]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)
      inline def flatMap(inline f: A => Query[B]): Query[B] = xs.flatMap(f)

  class QueryFor extends For[Query]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)
      inline def flatMap(inline f: A => Query[B]): Query[B] = xs.flatMap(f)
      inline def withFilter(inline f: A => Boolean): Query[A] = xs.withFilter(f)

  inline given listFunctor as ListFunctor = new ListFunctor
  inline given queryFunctor as QueryFunctor = new QueryFunctor

  inline given listMonad as ListMonad = new ListMonad
  inline given queryMonad as QueryMonad = new QueryMonad

  inline given listFor as ListFor = new ListFor
  inline given queryFor as QueryFor = new QueryFor

  extension [F[_], A, B](inline from: F[A])(using inline fun: Functor[F]) {
    inline def mapF(inline f: A => B) = from.map(f)
  }

  extension [F[_], A, B](inline from: F[A])(using inline fun: Monad[F]) {
    inline def mapM(inline f: A => B) = from.map(f)
    inline def flatMapM(inline f: A => F[B]) = from.flatMap(f)
  }
  
  object UseCase:
    extension [F[_]](inline people: F[Person])(using inline fun: For[F]):
      inline def joesAddresses(inline addresses: F[Address]) =
        for {
          p <- people if (p.name == "Joe")
          a <- addresses if (p.id == a.fk)
        } yield (p, a)

  def main(args: Array[String]): Unit = {
    inline def people: Query[Person] = query[Person]
    inline def addresses: Query[Address] = query[Address]

    //println( List(1,2,3).mapF(i => i + 1) )
    //println( List(1,2,3).flatMapM(i => List(i, i+1)) )

    //inline def q = quote { people.mapF(p => p.name) }
    //inline def q = quote { people.flatMapM(p => query[Address]) }
    //println( run(q).string )

    val peopleL = List(Person(1, "Joe", 22), Person(2, "Jack", 33), Person(3, "James", 44))
    val addressesL = List(Address(1, "123 St.", 111), Address(2, "456 St.", 222), Address(3, "789 St.", 333))

    import UseCase._

    println(peopleL.joesAddresses(addressesL))
    inline def q = quote { people.joesAddresses(addresses) }
    println( run(q) )


  }
}
// hellooooooooooo