package io.getquill.generic

import scala.quoted._
import io.getquill.MappedEncoding
import io.getquill.util.Format

// implicit def mappedEncoder[Mapped, Base](implicit mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]

trait AnyValEncoderContext[Encoder[_], Mapped] {
  def mappedEncoder[Base](mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]
}

trait AnyValDecoderContext[Decoder[_], Mapped] {
  def mappedDecoder[Base](mapped: MappedEncoding[Base, Mapped], decoder: Decoder[Base]): Decoder[Mapped]
}

object MappedDecoderMaker:
  inline def apply[Decoder[_], Mapped <: AnyVal](ctx: AnyValDecoderContext[Decoder, Mapped]): Decoder[Mapped] = ${ applyImpl[Decoder, Mapped]('ctx) }
  def applyImpl[Decoder[_]: Type, Mapped <: AnyVal: Type](ctx: Expr[AnyValDecoderContext[Decoder, Mapped]])(using qctx: Quotes): Expr[Decoder[Mapped]] =
    import qctx.reflect._
    // try to summon a normal encoder first and see if that works
    Expr.summon[Decoder[Mapped]] match
      case Some(decoder) => decoder
      case None =>
        // get the type from the primary constructor and try to summon an encoder for that
        val tpe = TypeRepr.of[Mapped]
        val constructor = tpe.typeSymbol.primaryConstructor
        // TODO Better error describing why the encoder could not be syntheisized if the constructor doesn't exist or has wrong form (i.e. != 1 arg)
        val firstParam = tpe.typeSymbol.primaryConstructor.paramSymss(0)(0)
        val firstParamField = tpe.typeSymbol.memberField(firstParam.name)
        val firstParamType = tpe.memberType(firstParamField)
        // Try to summon an encoder from the first param type
        firstParamType.asType match
          case '[tt] =>
            Expr.summon[Decoder[tt]] match
              case Some(enc) => 
                val mappedDecoding =
                  '{ MappedEncoding((v: tt) => ${
                    Apply(
                      Select(New(TypeTree.of[Mapped]), constructor),
                      List('v.asTerm)
                    ).asExprOf[Mapped]
                  }) }
                '{ $ctx.mappedDecoder[tt]($mappedDecoding, $enc) }
              case None => 
                report.throwError(
                  s"Cannot find a regular encoder for the AnyVal type ${Format.TypeRepr(tpe)} or a mapped-encoder for it's base type: ${Format.TypeRepr(firstParamType)}"
                )

object MappedEncoderMaker:
  inline def apply[Encoder[_], Mapped <: AnyVal](ctx: AnyValEncoderContext[Encoder, Mapped]): Encoder[Mapped] = ${ applyImpl[Encoder, Mapped]('ctx) }
  def applyImpl[Encoder[_]: Type, Mapped <: AnyVal: Type](ctx: Expr[AnyValEncoderContext[Encoder, Mapped]])(using qctx: Quotes): Expr[Encoder[Mapped]] =
    import qctx.reflect._
    // try to summon a normal encoder first and see if that works
    Expr.summon[Encoder[Mapped]] match
      case Some(encoder) => encoder
      case None =>
        // get the type from the primary constructor and try to summon an encoder for that
        val tpe = TypeRepr.of[Mapped]
        // TODO Better error describing why the encoder could not be syntheisized if the constructor doesn't exist or has wrong form (i.e. != 1 arg)
        val firstParam = tpe.typeSymbol.primaryConstructor.paramSymss(0)(0)
        val firstParamField = tpe.typeSymbol.memberField(firstParam.name)
        val firstParamType = tpe.memberType(firstParamField)
        // Try to summon an encoder from the first param type
        firstParamType.asType match
          case '[tt] =>
            Expr.summon[Encoder[tt]] match
              case Some(enc) => 
                val mappedEncoding = '{ MappedEncoding((v:Mapped) => ${ Select('v.asTerm, firstParamField).asExprOf[tt] }) }
                '{ $ctx.mappedEncoder[tt]($mappedEncoding, $enc) }
              case None => 
                report.throwError(
                  s"Cannot find a regular encoder for the AnyVal type ${Format.TypeRepr(tpe)} or a mapped-encoder for it's base type: ${Format.TypeRepr(firstParamType)}"
                )

object AnyValToValMacro:
  // For: Foo(value: String) extends AnyVal
  // Cls := Foo, V := String
  inline def apply[Cls <: AnyVal, V]: MappedEncoding[Cls, V] = ${ applyImpl[Cls, V] }
  def applyImpl[Cls <: AnyVal: Type, V: Type](using qctx: Quotes): Expr[MappedEncoding[Cls, V]] =
    import qctx.reflect._
    val tpe = TypeRepr.of[Cls]
    val firstParam = tpe.typeSymbol.primaryConstructor.paramSymss(0)(0)
    val firstParamField = tpe.typeSymbol.memberField(firstParam.name)
    val firstParamType = tpe.memberType(firstParamField)
    println("Member type of 1st param: " + io.getquill.util.Format.TypeRepr(firstParamType))
    '{ MappedEncoding((v:Cls) => ${ Select('v.asTerm, firstParamField).asExprOf[V] }) }

object ValToAnyValMacro:
  // For: String, Foo(value: String) extends AnyVal
  // V := String, Cls := Foo
  inline def apply[V, Cls <: AnyVal]: MappedEncoding[V, Cls] = ${ applyImpl[V, Cls] }
  def applyImpl[V: Type, Cls <: AnyVal: Type](using qctx: Quotes): Expr[MappedEncoding[V, Cls]] =
    import qctx.reflect._
    val tpe = TypeRepr.of[Cls]
    val constructor = tpe.typeSymbol.primaryConstructor
    '{ MappedEncoding((v: V) => ${
      Apply(
        Select(New(TypeTree.of[Cls]), constructor),
        List('v.asTerm)
      ).asExprOf[Cls]
     }) }