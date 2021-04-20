package io.getquill.generic

import scala.quoted._
import io.getquill.MappedEncoding

object AnyValToValMacro:
  // For: Foo(value: String) extends AnyVal
  // Cls := Foo, V := String
  inline def apply[Cls <: AnyVal, V]: MappedEncoding[Cls, V] = ${ applyImpl[Cls, V] }
  def applyImpl[Cls <: AnyVal: Type, V: Type](using qctx: Quotes): Expr[MappedEncoding[Cls, V]] =
    import qctx.reflect._
    val tpe = TypeRepr.of[Cls]
    val firstParam = tpe.typeSymbol.primaryConstructor.paramSymss(0)(0)
    val firstParamField = tpe.typeSymbol.memberField(firstParam.name)
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