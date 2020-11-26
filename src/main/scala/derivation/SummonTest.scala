// package derivation

// import SummonJsonEncoderTest._

// object SummonTest {
//   val stuff = ThePerson("Joe", 123, List(TheAddress("123 Street"))) 

//   def main(args: Array[String]):Unit = {
//     //println( SummonJsonEncoderTest.encodeFromSummonedMirror[ThePerson](stuff) )

//     // This works but without address being there
//     //println( SummonJsonEncoderManualTest.summonMirrorAndManuallyEncode[ThePerson](stuff) )

//     println( SummonJsonEncoderTest.usingSummonFrom[ThePerson](SummonTest.stuff) )

//     //println( SummonJsonEncoder.encodeAndMessAroundTerm(Simple.stuff) ) //hello
//   }
// }