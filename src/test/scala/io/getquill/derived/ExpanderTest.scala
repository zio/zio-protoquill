package io.getquill.derived

import org.junit.Test
import org.junit.Assert._
import io.getquill.ast._

class ExpanderTest {
  import io.getquill.derived.Expander._
  
  @Test def simple(): Unit = {
    case class Nested(i: Int, l: Long) derives Expander
    case class Entity(a: String, b: Nested) derives Expander
    val exp = summon[Expander[Entity]]
    println( exp.expand(Term("x")) )

    //assertEquals(exp.expand(Ident("x")).toString, "List(x.a, x.b.i, x.b.l)")
  }

  @Test def nestedOptional(): Unit = {
    case class Nested(i: Int, l: Long) derives Expander
    case class Entity(a: String, b: Option[Nested]) derives Expander
    val exp = summon[Expander[Entity]]
    val expansion = exp.expand(Term("x"))
    println( expansion )
    assertEquals( "List(x.a, x.b.map(v => v.i), x.b.map(v => v.l))", expansion.toAst.toString )
  }

  @Test def tuple(): Unit = {
    case class Entity(a: String, b: Int) derives Expander
    // Can't do that because can't just summon inner expanders e.g. because Option needs to know the field
    // name that it was created from.
    // TODO This should be  possible. Maybe do something like:
    // given quillExpander[T](given encoder: Encoder[T]): Expander[T] = Expander.derived
    // or alternatively, maybe use the original 'derives quill' approach
    // given quillExpander[T](given quill: Quill[A]): Expander[T] = Expander.derived
    // given tup2Expander[A, B](given Expander[A], Expander[B]): Expander[(A, B)] = Expander.derived
    // in fact, this is true of any scalar

    // TODO This REALLY has to be done inside of a macro. Need to check against latest
    // version of dotty if that is possible.
    given tup2Expander: Expander[(String, Option[Entity])] = Expander.derived
    val exp = summon[Expander[(String, Option[Entity])]] //heloooo
    println( exp.expand(Term("x")) )

    //assertEquals( exp.expand(Ident("x")).toString, "List(x._1, x._2.map(v => v.a), x._2.map(v => v.b))" )
  }
}
