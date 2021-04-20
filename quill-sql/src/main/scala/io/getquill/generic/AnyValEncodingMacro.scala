package io.getquill.generic

import scala.quoted._
import io.getquill.MappedEncoding
import io.getquill.util.Format

// implicit def mappedEncoder[Mapped, Base](implicit mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): Encoder[Mapped]

trait AnyValEncoderContext[AnyValEncoder[_], Encoder[_], Mapped] {
  def mappedEncoder[Base](mapped: MappedEncoding[Mapped, Base], encoder: Encoder[Base]): AnyValEncoder[Mapped]
}

trait AnyValDecoderContext[AnyValDecoder[_], Decoder[_], Mapped] {
  def mappedDecoder[Base](mapped: MappedEncoding[Base, Mapped], decoder: Decoder[Base]): AnyValDecoder[Mapped]
}

object MappedDecoderMaker:
  inline def apply[AnyValDecoder[_], Decoder[_], Mapped <: AnyVal](ctx: AnyValDecoderContext[AnyValDecoder, Decoder, Mapped]): AnyValDecoder[Mapped] = ${ applyImpl[AnyValDecoder, Decoder, Mapped]('ctx) }
  def applyImpl[AnyValDecoder[_]: Type, Decoder[_]: Type, Mapped <: AnyVal: Type](ctx: Expr[AnyValDecoderContext[AnyValDecoder, Decoder, Mapped]])(using qctx: Quotes): Expr[AnyValDecoder[Mapped]] =
    import qctx.reflect._
    // try to summon a normal encoder first and see if that works
    Expr.summon[Decoder[Mapped]] match
      case Some(decoder) if !(decoder.asTerm.tpe <:< TypeRepr.of[AnyValDecoder[Any]]) =>
        decoder.asExprOf[AnyValDecoder[Mapped]]
      case _ =>
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
                '{ $ctx.mappedDecoder[tt]($mappedDecoding, $enc) }.asExprOf[AnyValDecoder[Mapped]]
              case None => 
                report.throwError(
                  s"Cannot find a regular encoder for the AnyVal type ${Format.TypeRepr(tpe)} or a mapped-encoder for it's base type: ${Format.TypeRepr(firstParamType)}"
                )

object MappedEncoderMaker:
  inline def apply[AnyValEncoder[_], Encoder[_], Mapped <: AnyVal](ctx: AnyValEncoderContext[AnyValEncoder, Encoder, Mapped]): AnyValEncoder[Mapped] = ${ applyImpl[AnyValEncoder, Encoder, Mapped]('ctx) }
  def applyImpl[AnyValEncoder[_]: Type, Encoder[_]: Type, Mapped <: AnyVal: Type](ctx: Expr[AnyValEncoderContext[AnyValEncoder, Encoder, Mapped]])(using qctx: Quotes): Expr[AnyValEncoder[Mapped]] =
    import qctx.reflect._
    // try to summon a normal encoder first and see if that works
    Expr.summon[Encoder[Mapped]] match
      case Some(encoder) if !(encoder.asTerm.tpe <:< TypeRepr.of[AnyValEncoder[Any]]) =>
        encoder.asExprOf[AnyValEncoder[Mapped]]
      case _ =>
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
                '{ $ctx.mappedEncoder[tt]($mappedEncoding, $enc) }.asExprOf[AnyValEncoder[Mapped]]
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