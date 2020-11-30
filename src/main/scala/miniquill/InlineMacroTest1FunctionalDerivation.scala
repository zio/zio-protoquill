package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import scala.compiletime.{erasedValue, summonFrom, constValue}

object InlineMacroTest1FunctionalDerivation {
  import io.getquill._
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._
  import io.getquill.derived.MapProc

  case class Person(firstName: String, lastName: String)

  inline def q = quote {
    query[Person].filter(p => MapProc(p))
  }

  println( run(q).string )
  
}
