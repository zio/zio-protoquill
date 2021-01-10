package io.getquill.util

import scala.quoted._

case class Bar(value: String)
case class Foo(value: String):
  def bar: Bar = Bar(value)

object Mac {
  inline def run(inline foo: Foo): Bar = ${ runImpl('foo) }
  def runImpl(foo: Expr[Foo])(using Quotes): Expr[Bar] = {
    import quotes.reflect._
    println(pprint.apply(foo.asTerm))
    '{ $foo.bar }
  } 
}