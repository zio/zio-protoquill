package miniquill.dsl

import scala.reflect.ClassTag
import miniquill.context.mirror._

@main def expanderNested() = {
  import io.getquill.ast._

  {
    case class Nested(i: Int, l: Long) derives Expander
    case class Entity(a: String, b: Nested) derives Expander
    val exp = summon[Expander[Entity]]
    println( exp.expand(Ident("x")) )
  }

  {
    case class Nested(i: Int, l: Long) derives Expander
    case class Entity(a: String, b: Option[Nested]) derives Expander
    val exp = summon[Expander[Entity]]
    println( exp.expand(Ident("x")) )
  }

  {
    case class Entity(a: String, b: Int) derives Expander
    // Can't do that because can't just summon inner expanders e.g. because Option needs to know the field
    // name that it was created from.
    // given tup2Expander[A, B](given Expander[A], Expander[B]): Expander[(A, B)] = Expander.derived
    // in fact, this is true of any scalar

    // TODO This REALLY has to be done inside of a macro. Need to check against latest
    // version of dotty if that is possible.
    given tup2Expander: Expander[(String, Option[Entity])] = Expander.derived
    val exp = summon[Expander[(String, Option[Entity])]] //heloooo
    println( exp.expand(Ident("x")) )
  }

}

@main def tryDecode() = {

  class MDec extends MirrorDecoders
  val mdec = new MDec()
  import mdec._

  // TODO Can I override the decoders like this?
  // case class MirrorDecoder[T](decoder: GenericDecoder[Row, T]) extends GenericDecoder[Row, T] {
  //   override def apply(index: Int, row: Row) =
  //     decoder(index, row)
  // }
  // def decoder[T: ClassTag]: GenericDecoder[Row, T] = MirrorDecoder((index: Int, row: Row) => row[T](index))
  // implicit val stringDecoder: GenericDecoder[Row, String] = decoder[String]
  // implicit val intDecoder: GenericDecoder[Row, Int] = decoder[Int]

  {
    case class OneLevel(name: String, age: Int)
    val r = Row("foo", 1)
    given GenericDecoder[Row, OneLevel] = GenericDecoder.derived
    val dec = summon[GenericDecoder[Row, OneLevel]]
    println( dec(0, r) )
  }

  

  case class TwoLevelInner(foo: String, bar: String)
  case class TwoLevelOuter(name: String, age: Int, emb: TwoLevelInner, stuff: String)
  val rr = Row("foo", 1, "blah", "blahblah", "stuff")
  given GenericDecoder[Row, TwoLevelInner] = GenericDecoder.derived
  given GenericDecoder[Row, TwoLevelOuter] = GenericDecoder.derived
   // TODO if don't include this inner one, error is very cryptic. Need to improve!
  val decDec = summon[GenericDecoder[Row, TwoLevelOuter]]
  println( decDec(0, rr) )
}



