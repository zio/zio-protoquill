package io.getquill.derived

import org.junit.Test
import org.junit.Assert._
import io.getquill.ast._

class ExpanderTest {
  import io.getquill.derived.Expander._
  val body = Ident("body")

  @Test def personWithEmbeddedAddress(): Unit = {
    case class Address(street: String)
    case class Person(name: String, address: Address)
    val exp = Expander.runtime[Person](body)
  }
  
  @Test def simple(): Unit = {
    case class Nested(i: Int, l: Long)
    case class Entity(a: String, b: Nested)
    val ast = Expander.runtime[Entity](body)
    assertEquals( "body.map(x => (x.a, x.b.i, x.b.l))", ast.toString )
  }

  @Test def singleFieldNestedOptional(): Unit = {
    case class Nested(i: Int)
    case class Entity(a: Option[Nested])
    val ast = Expander.runtime[Entity](body)
    assertEquals( "body.map(x => x.a.map(v => v.i))", ast.toString )
  }

  @Test def nestedOptional(): Unit = {
    case class Nested(i: Int, l: Long)
    case class Entity(a: String, b: Option[Nested])
    val ast = Expander.runtime[Entity](body)
    assertEquals( "body.map(x => (x.a, x.b.map(v => v.i), x.b.map(v => v.l)))", ast.toString )
  }

  @Test def nestedOptionalMulti(): Unit = {
    case class ReallyNested(foo: Int, bar: Int)
    case class Nested(i: Int, l: Option[ReallyNested])
    case class Entity(a: String, b: Option[Nested])
    val ast = Expander.runtime[Entity](body)
    assertEquals( "body.map(x => (x.a, x.b.map(v => v.i), x.b.map(v => v.l.map(v => v.foo)), x.b.map(v => v.l.map(v => v.bar))))", ast.toString )
  }

  @Test def tuple(): Unit = {
    val ast = Expander.runtime[(String, String)](body)
    assertEquals( "body.map(x => (x._1, x._2))", ast.toString )
  }

  @Test def nestedTuple(): Unit = {
    case class Entity(a: String, b: Int)
    val ast = Expander.runtime[(String, Option[Entity])](body)
    assertEquals( "body.map(x => (x._1, x._2.map(v => v.a), x._2.map(v => v.b)))", ast.toString )
  }
}
