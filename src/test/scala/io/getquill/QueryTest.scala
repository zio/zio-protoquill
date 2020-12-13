package io.getquill

import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter.Quoted
import miniquill.quoter._
import io.getquill._
import io.getquill.ast._
import miniquill.quoter.QuotationLot
import miniquill.quoter.QuotationVase
import io.getquill.context.ExecutionType
import org.scalatest._

class QueryTest {
  import io.getquill.context.ContextRet

  case class Person(name: String, age: Int)
  
  inline def mapping = QuotingSimple.quote // works if you turn this into a regular def!!!
  
  //val ctx = new MirrorContext(MirrorIdiom, Literal)
  def main(args: Array[String]): Unit = {
    println( ContextRet.runAndTest( mapping ) ) //hellooooooooooooooooooooooooooooooooooooo
  }

}