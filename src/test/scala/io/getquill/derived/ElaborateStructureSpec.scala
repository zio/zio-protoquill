package io.getquill.derived

import io.getquill.ast._
import io.getquill.Spec
import io.getquill.quat.Quat
import io.getquill.derived.ElaborateStructureExt.TaggedSplicedCaseClass
import scala.quoted.Expr

object Blah {
  // but there will be one from here
  sealed trait Shape
  case class Rectangle(width: Int, height: Int) extends Shape
  case class Circle(radius: Int) extends Shape
}

class ElaborateProductValueStructureSpec extends Spec {
  extension (tscc: TaggedSplicedCaseClass)
    def pullout: (Ast, List[(String, Any)]) = (tscc.ast, tscc.lifts)

  "elaboration of product value should work for" - {
    "simple class" in {
      case class Person(name: String, age: Int)
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Person("Joe", 123)).pullout
      ast mustEqual CaseClass(List(("name", ScalarTag("name")), ("age", ScalarTag("age"))))
      lifts mustEqual List(("name","Joe"), ("age",123))
    }
    // Starting to use string representation here for comparison because AST expressions are getting cumbersome
    // keeping this example with both forms for reference
    "Entity with Nested" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Nested)
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", Nested(1, 2L))).pullout
      // Regular AST form
      ast mustEqual CaseClass(List(("a", ScalarTag("a")), ("b", CaseClass(List(("i", ScalarTag("bi")), ("l", ScalarTag("bl")))))))
      // Stringified form
      ast.toString mustEqual "CaseClass(a: lift(a), b: CaseClass(i: lift(bi), l: lift(bl)))"
      lifts mustEqual List(("a", "foo"), ("bi", 1), ("bl", 2))
    }

    "Nested with optional fields" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", Some(Nested(1, 2L)))).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: lift(bl))))"
      lifts mustEqual List(("a", "foo"), ("bi", Some(1)), ("bl", Some(2)))
    }

    "Nested with optional fields (None)" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", None)).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: lift(bl))))"
      lifts mustEqual List(("a", "foo"), ("bi", None), ("bl", None))
    }

    "Triple-nested with optional fields" in {
      case class ReallyNested(one: Int, two: Int)
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", Some(Nested(1, Some(ReallyNested(2, 3)))))).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: Some(CaseClass(one: lift(blone), two: lift(bltwo))))))"
      lifts mustEqual List(("a","foo"), ("bi",Some(1)), ("blone",Some(2)), ("bltwo",Some(3)))
    }

    "Quadruple-nested with optional fields" in {
      case class ReallyReallyNested(alpha: Int, beta: Int)
      case class ReallyNested(one: Int, two: Option[ReallyReallyNested])
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", Some(Nested(1, Some(ReallyNested(2, Some(ReallyReallyNested(3, 4)))))))).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: Some(CaseClass(one: lift(blone), two: Some(CaseClass(alpha: lift(bltwoalpha), beta: lift(bltwobeta))))))))"
      lifts mustEqual List(("a","foo"), ("bi",Some(1)), ("blone",Some(2)), ("bltwoalpha",Some(3)), ("bltwobeta",Some(4)))
    }

    "Quadruple-nested with optional fields (None)" in {
      case class ReallyReallyNested(alpha: Int, beta: Int)
      case class ReallyNested(one: Int, two: Option[ReallyReallyNested])
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", None)).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: Some(CaseClass(one: lift(blone), two: Some(CaseClass(alpha: lift(bltwoalpha), beta: lift(bltwobeta))))))))"
      lifts mustEqual List(("a","foo"), ("bi",None), ("blone",None), ("bltwoalpha",None), ("bltwobeta",None))
    }

    "Quadruple-nested with optional fields Some/None" in {
      case class ReallyReallyNested(alpha: Int, beta: Int)
      case class ReallyNested(one: Int, two: Option[ReallyReallyNested])
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val (ast, lifts) = ElaborateStructureExt.ofProductValueExternal(Entity("foo", Some(Nested(1, None)))).pullout
      ast.toString mustEqual "CaseClass(a: lift(a), b: Some(CaseClass(i: lift(bi), l: Some(CaseClass(one: lift(blone), two: Some(CaseClass(alpha: lift(bltwoalpha), beta: lift(bltwobeta))))))))"
      lifts mustEqual List(("a","foo"), ("bi",Some(1)), ("blone",None), ("bltwoalpha",None), ("bltwobeta",None))
    }

    // "Tuple" in {
    //   val ast = ElaborateStructureExt.external[(String, String)](body)
    //   ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2: x._2))"
    // }

    // "Nested Tuple" in {
    //   case class Entity(a: String, b: Int)
    //   val ast = ElaborateStructureExt.external[(String, Option[Entity])](body)
    //   ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2a: x._2.map((v) => v.a), _2b: x._2.map((v) => v.b)))"
    // }
  }
}

class ElaborateStructureSpec extends Spec {
  import io.getquill.derived.ElaborateStructure._
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

    val v = ElaborateStructureExt.external[Blah.Shape](body)
    println(v)
  }

  "correct expansion for" - {

    "Person with Embedded Address" in {
      case class Address(street: String)
      case class Person(name: String, address: Address)
      val exp = ElaborateStructureExt.external[Person](body)
    }
    
    "Entity with Nestsed" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Nested)
      val ast = ElaborateStructureExt.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.i, bl: x.b.l))"
    }

    "Nested with optional field" in {
      case class Nested(i: Int)
      case class Entity(a: Option[Nested])
      val ast = ElaborateStructureExt.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => x.a.map((v) => v.i))"
    }

    "Nested with optional and multiple fields" in {
      case class Nested(i: Int, l: Long)
      case class Entity(a: String, b: Option[Nested])
      val ast = ElaborateStructureExt.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.map((v) => v.i), bl: x.b.map((v) => v.l)))"
    }

    "Triple-nested with optional fields" in {
      case class ReallyNested(foo: Int, bar: Int)
      case class Nested(i: Int, l: Option[ReallyNested])
      case class Entity(a: String, b: Option[Nested])
      val ast = ElaborateStructureExt.external[Entity](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(a: x.a, bi: x.b.map((v) => v.i), blfoo: x.b.map((v) => v.l.map((v) => v.foo)), blbar: x.b.map((v) => v.l.map((v) => v.bar))))"
    }

    "Tuple" in {
      val ast = ElaborateStructureExt.external[(String, String)](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2: x._2))"
    }

    "Nested Tuple" in {
      case class Entity(a: String, b: Int)
      val ast = ElaborateStructureExt.external[(String, Option[Entity])](body)
      ast.toString  mustEqual  "body.map(x => CaseClass(_1: x._1, _2a: x._2.map((v) => v.a), _2b: x._2.map((v) => v.b)))"
    }
  }
}
