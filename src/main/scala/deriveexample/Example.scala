package deriveexample

import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}

inline def summonEq[T]: Eq[T] =
  summonFrom {
    case eq: Eq[T] =>  eq
  }

inline def summonAll[T <: Tuple]: List[Eq[_]] = inline erasedValue[T] match {
  case _: EmptyTuple => Nil
  case _: (t *: ts) => summonEq[t] :: summonAll[ts]
}

trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given Eq[Int] {
    def eqv(x: Int, y: Int) = x == y
  }

  def check(elem: Eq[_])(x: Any, y: Any): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = {
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)
      }
    }

  def eqProduct[T](p: Mirror.ProductOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }
    }

  inline def derived[T]: Eq[T] =
    summonFrom {
      case m: Mirror.Of[T] =>
        val elemInstances = summonAll[m.MirroredElemTypes]
        inline m match {
          case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
          case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
        }
    }

  
}

object AutomaticEq {
  inline implicit def autoEq[T]: Eq[T] = Eq.derived
  inline def [T](a: T) === (b: T) = autoEq[T].eqv(a, b)
}