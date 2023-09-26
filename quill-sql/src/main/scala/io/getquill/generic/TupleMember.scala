package io.getquill.generic

import scala.quoted._

// object ZeroArgsMethod:
//    def unapply()

object TupleMember {
  inline def apply[T](inline matchMember: String): Unit = ${ impl[T]('matchMember) }
  def impl[T: Type](matchMemberExpr: Expr[String])(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    val matchMember =
      matchMemberExpr match {
        case Expr(str) => str
        case _         => report.throwError("Not a static string")
      }

    sealed trait ElaboratedField
    object ElaboratedField {
      def apply(tpe: TypeRepr, fieldName: String) = {
        val typeSymbol = tpe.typeSymbol
        typeSymbol.methodMembers.find(m => m.name == fieldName && m.paramSymss == List()).map(ZeroArgsMethod(_))
          .orElse(typeSymbol.fieldMembers.find(m => m.name == fieldName).map(Field(_)))
          .getOrElse(NotFound)
      }

      case class ZeroArgsMethod(symbol: Symbol) extends ElaboratedField
      case class Field(symbol: Symbol) extends ElaboratedField
      case object NotFound extends ElaboratedField
    } // end ElaboratedField

    val clsType = TypeRepr.of[T]
    // val memberSymbol = clsType.widen.classSymbol.get.memberField("_1")
    // val memberSymbol = clsType.widen.typeSymbol.memberField("_1")
    // val memberSymbol = clsType.typeSymbol.memberField("_1")
    // val memberSymbol = clsType.typeSymbol.fieldMember("_1")
    val elab = ElaboratedField(clsType, matchMember)
    elab match {
      case ElaboratedField.ZeroArgsMethod(sym) => report.info(s"${sym} is a zero-args member whose type is ${clsType.widen.memberType(sym).widen}")
      case ElaboratedField.Field(sym)          => report.info(s"${sym} is a field whose type is ${clsType.widen.memberType(sym).widen}")
      case ElaboratedField.NotFound            => report.info(s"${matchMember} was not found")
    }

    '{ () }
  }
}
