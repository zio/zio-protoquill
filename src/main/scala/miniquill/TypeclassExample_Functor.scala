package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object TypeclassExample_Functor {
  import io.getquill._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(id: Int, name: String, age: Int)

  trait Functor[F[_]]:
    extension [A, B](inline x: F[A]):
      inline def map(inline f: A => B): F[B]

  class ListFunctor extends Functor[List]:
    extension [A, B](inline xs: List[A])
      inline def map(inline f: A => B): List[B] = xs.map(f)

  class QueryFunctor extends Functor[Query]:
    extension [A, B](inline xs: Query[A])
      inline def map(inline f: A => B): Query[B] = xs.map(f)

  inline given listFunctor as ListFunctor = new ListFunctor
  inline given queryFunctor as QueryFunctor = new QueryFunctor

  extension [F[_], A, B](inline from: F[A])(using inline fun: Functor[F]) {
    inline def mapF(inline f: A => B) = from.map(f)
  }

  def main(args: Array[String]): Unit = {
    inline def people: Query[Person] = query[Person]
    println( List(1,2,3).mapF(i => i + 1) )
    inline def q = quote { people.mapF(p => p.name) }
    println( run(q) )
  }
}
