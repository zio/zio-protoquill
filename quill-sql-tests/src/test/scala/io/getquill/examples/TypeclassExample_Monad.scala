package io.getquill.examples

import scala.language.implicitConversions
import scala.compiletime.{erasedValue, summonFrom, constValue}
import io.getquill._
import MirrorContext.Codec.*

object TypeclassExample_Monad {

  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(id: Int, name: String, age: Int)
  case class Address(street: String, zip: Int) extends Embedded
  given CompositeDecoder[Person] = deriveComposite
  given CompositeDecoder[Address] = deriveComposite

  trait Monad[F[_]] { // extends Functor[F]
    //inline def pure[A](x: A): F[A]
    extension [A, B](inline x: F[A]) {
      inline def map(inline f: A => B): F[B]
      inline def flatMap(inline f: A => F[B]): F[B]
    }
  }

  inline given Monad[List] with {
    extension [A, B](inline xs: List[A]) {
      inline def map(inline f: A => B): List[B] = xs.map(f)
      inline def flatMap(inline f: A => List[B]): List[B] = xs.flatMap(f)
    }
  }

  inline given Monad[Query] with {
    extension [A, B](inline xs: Query[A]) {
      inline def map(inline f: A => B): Query[B] = xs.map(f)
      inline def flatMap(inline f: A => Query[B]): Query[B] = xs.flatMap(f)
    }
  }

  extension [F[_], A, B](inline from: F[A])(using inline fun: Monad[F]) {
    inline def mapM(inline f: A => B) = from.map(f)
    inline def flatMapM(inline f: A => F[B]) = from.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    inline def people: Query[Person] = query[Person]
    inline def addresses: Query[Address] = query[Address]

    println( List(1,2,3).mapM(i => i + 1) )
    println( List(1,2,3).flatMapM(i => List(i, i+1)) )

    inline def q1 = quote { people.mapM(p => p.name) }
    inline def q2 = quote { people.flatMapM(p => query[Address]) }
    println( run(q1).string )
    println( run(q2).string )
  }
}
