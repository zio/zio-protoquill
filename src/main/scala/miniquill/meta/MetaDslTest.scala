package miniquill.meta

import DecoderDsl._



@main def tryDecode() = {
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



