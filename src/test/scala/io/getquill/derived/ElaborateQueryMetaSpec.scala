package io.getquill.derived

import io.getquill.ast._
import io.getquill.Spec
import io.getquill.quat.Quat

object Blah {
  // but there will be one from here
  sealed trait Shape
  case class Rectangle(width: Int, height: Int) extends Shape
  case class Circle(radius: Int) extends Shape
}

class ElaborateQueryMetaSpec extends Spec {
  import io.getquill.derived.ElaborateQueryMeta._
  val body = Ident("body", Quat.Generic)

  // If from here, there will be no product mirror
  // sealed trait Shape
  // case class Rectangle(width: Int, height: Int) extends Shape
  // case class Circle(radius: Int) extends Shape

  // enum Shape:
  //   case Rectangle(width: Int, height: Int) extends Shape
  //   case Circle(radius: Int) extends Shape

  "coproduct" - {  
    // TODO test nested embedded classes in the coproducts

    
    //   // implicit inline def genDec[ResultRow, T]: GenericDecoder[T] = ${ GenericDecoder.derived[ResultRow, T] }

    val v = ElaborateQueryMetaHook.external[Blah.Shape](body)
    println(v)
  }

  "correct expansion for" - {

    "Person with Embedded Address" in {
      case class Address(street: String)
      case class Person(name: String, address: Address)
      val exp = ElaborateQueryMetaHook.external[Person](body)
    }
    
    "Entity with Nestsed" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Nested)
      val ast = ElaborateQueryMetaHook.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.i, bl: x.b.l))"
    }

    "Nested with optional field" in {
      case class Nested(i: Int)
      case class Entity(a: Option[Nested])
      val ast = ElaborateQueryMetaHook.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => x.a.map((v) => v.i))"
    }

    "Nested with optional and multiple fields" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Option[Nested])
      val ast = ElaborateQueryMetaHook.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.map((v) => v.i), bl: x.b.map((v) => v.l)))"
    }

    "Triple-nested with optional fields" in {
      case class ReallyNested(foo: Int, bar: Int)
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val ast = ElaborateQueryMetaHook.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.map((v) => v.i), blfoo: x.b.map((v) => v.l.map((v) => v.foo)), blbar: x.b.map((v) => v.l.map((v) => v.bar))))"
    }

    "Tuple" in {
      val ast = ElaborateQueryMetaHook.external[(String, String)](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2: x._2))"
    }

    "Nested Tuple" in {
      case class Entity(a: String, b: Int)
      val ast = ElaborateQueryMetaHook.external[(String, Option[Entity])](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2a: x._2.map((v) => v.a), _2b: x._2.map((v) => v.b)))"
    }
  }
}
