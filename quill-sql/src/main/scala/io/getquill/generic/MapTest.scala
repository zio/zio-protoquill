package io.getquill.generic

import scala.reflect.TypeTest

object MapTest {
  type KeyMap[K] = (Map[K, _], Map[K, _])

  def main(args: Array[String]): Unit = {

    // val a = Map[String, Int]("foo" -> 1)
    // val b = Map[String, Int]("bar" -> 2)
    // // val c = Map[String, String]("bar" -> "baz")

    // "fooo" match {
    //   case _: KeyMap[k] => println("Yay") ////
    //   case _ => println("Nay")
    // }
  }



  //given TypeTest[Foo, Bar] = ???

  trait Compare[X, Y] {
    def compare(x: X)(using TypeTest[X, Y]): Boolean =
      x match {
        case y: Y => true
        case _ => false
      }
  }

  trait CompareList[X, Y] {
    def compare(x: List[X])(using TypeTest[List[X & Y], List[Y]],
      TypeTest[List[X], List[X & Y]],
      TypeTest[List[X], List[Y]],
      TypeTest[X, Y],
      TypeTest[X & Y, Y],
      TypeTest[X, X & Y]
    ): Boolean =
      x match {
        case y: List[Y] => true
        case _ => false
      }
  }





}