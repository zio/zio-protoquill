package io.getquill

import io.getquill.util.debug.PrintMac

import IEDsl._

object UseIEDsl {
  case class Person(name: String, age: Int)

  def main(args: Array[String]): Unit = {
    inline def joes = query[Person].filter(p => p.name == "Joe")
    //PrintMac(joes)

    inline def qr = toQuote(joes)

    import io.getquill.lib._
    val ctx = new MirrorContext(MirrorSqlDialect, Literal)
    import ctx._

    def norm(ast: io.getquill.ast.Ast) = io.getquill.norm.BetaReduction(ast)
    println(run(qr).string)
    //println(io.getquill.util.Messages.qprint(norm(qr.ast))) //hellooooooo
    
  }
}