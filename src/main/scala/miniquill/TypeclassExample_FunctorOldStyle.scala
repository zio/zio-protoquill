package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object TypeclassExample_FunctorOldStyle {
  import io.getquill._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class Person(name: String, age: Int)

  trait Functor[F[_]]:
    inline def map[A, B](inline xs: F[A], inline f: A => B): F[B]

  class ListFunctor extends Functor[List]:
    inline def map[A, B](inline xs: List[A], inline f: A => B): List[B] = xs.map(f)
   
  class QueryFunctor extends Functor[Query]:
    inline def map[A, B](inline xs: Query[A], inline f: A => B): Query[B] = xs.map(f)

  inline given listFunctor as ListFunctor = new ListFunctor
  inline given queryFunctor as QueryFunctor = new QueryFunctor

  inline def doMap[F[_], A, B](inline from: F[A], inline f: A => B)(using inline fun: Functor[F]): F[B] =
    fun.map(from, f)

  def main(args: Array[String]): Unit = {
    val list2 = doMap(List(1,2,3), (i: Int) => i + 1)
    println(list2)

    inline def people: Query[Person] = query[Person]
    inline def q = quote { doMap(people, (p: Person) => p.name) }
    println( run(q).string )
  }
}
