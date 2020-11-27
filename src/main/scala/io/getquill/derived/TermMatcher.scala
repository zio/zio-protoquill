// package io.getquill.derived

// import miniquill.quoter._
// import scala.reflect.ClassTag
// import scala.compiletime.{erasedValue, summonFrom, constValue}
// import io.getquill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
// import scala.compiletime.erasedValue
// import io.getquill.ast.Visibility.{ Hidden, Visible }
// import scala.deriving._
// import scala.quoted._

// class TermBuilder(implicit val qctx: QuoteContext) {
//     import qctx.tasty.{Type => TType, Term => TTerm, Ident => TIdent, _}

//     def termBuilder[Fields, Types](id: TIdent, fieldsTup: Type[Fields], typesTup: Type[Types]): Expr[Boolean] = {
//       val ext = new TypeExtensions
//       import ext._

//       (fieldsTup, typesTup) match {
//         case ('[$field *: $fields], '[$tpe *: $types]) if (tpe.isProduct) =>
//           val unsealedClassSymbol = tpe.unseal.tpe.widen.classSymbol
//           val fieldString = field.constValue
//           val fieldMethod = unsealedClassSymbol.get.caseFields.filter(field => field.name == fieldString).head
//           val childTerm = Select(id, fieldMethod).seal.cast[String]
//           val expr = '{ (${childTerm}: String) == "hello" }
//           '{ ${expr} && ${termBuilder(id, fields, types)} }

//         case (_, '[EmptyTuple]) => '{ true }

//         case _ => report.throwError("Cannot Types In Expression Expression:\n" + (fieldsTup, typesTup))
//       }
//     }
//   }