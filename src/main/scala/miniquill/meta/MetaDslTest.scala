package miniquill.meta

import scala.reflect.ClassTag

@main def tryDecode() = {
  import DecoderDerivationDsl._

  // TODO so it seems like you can indeed put the leaf-node decoders into a different context
  case class MirrorDecoder[T](decoder: Decoder[T]) extends Decoder[T] {
    override def apply(index: Int, row: ResultRow) =
      decoder(index, row)
  }
  def decoder[T: ClassTag]: Decoder[T] = MirrorDecoder((index: Int, row: ResultRow) => row[T](index))
  implicit val stringDecoder: Decoder[String] = decoder[String]
  implicit val intDecoder: Decoder[Int] = decoder[Int]

  case class OneLevel(name: String, age: Int)
  val r = Row("foo", 1)
  given Decoder[OneLevel] = Decoder.derived
  val dec = summon[Decoder[OneLevel]]
  println( dec(0, r) )

  case class TwoLevelInner(foo: String, bar: String)
  case class TwoLevelOuter(name: String, age: Int, emb: TwoLevelInner, stuff: String)
  val rr = Row("foo", 1, "blah", "blahblah", "stuff")
  given Decoder[TwoLevelInner] = Decoder.derived
  given Decoder[TwoLevelOuter] = Decoder.derived
   // TODO if don't include this inner one, error is very cryptic. Need to improve!
  val decDec = summon[Decoder[TwoLevelOuter]]
  println( decDec(0, rr) )
}



