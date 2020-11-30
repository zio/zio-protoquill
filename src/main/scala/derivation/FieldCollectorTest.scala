// package derivation

// sealed trait Par(one: String, two:String) //derives FieldCollector ('derived needs to be a 'using' in this case???)
// case class Foo(one:String, two:String, foofoo:String) extends Par(one, two)
// case class Bar(one:String, two:String, barbar:String) extends Par(one, two)

// @main def fieldCollectionTest = {

//   given FieldCollector[Par] = FieldCollector.derived
//   given FieldCollector[Foo] = FieldCollector.derived
//   given FieldCollector[Bar] = FieldCollector.derived

//   val fc = summon[FieldCollector[Par]]
//   println( fc.collect(new Collector).fields ) //helloo
// }