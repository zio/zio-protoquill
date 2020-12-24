// package io.getquill

// import io.getquill.parser._
// import scala.quoted._
// 
// import io.getquill.ast._

// trait CustomParserFactory extends BaseParserFactory {
//   override def userDefined(using qctxInput: Quotes) = Parser(new ParserComponent {
//     val qctx = qctxInput
//     def apply(root: Parser) = PartialFunction.empty[Expr[_], Ast]
//   })
// }
// object CustomParserFactory extends CustomParserFactory