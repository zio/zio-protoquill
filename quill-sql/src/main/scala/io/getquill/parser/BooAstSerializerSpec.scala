package io.getquill.parser

import io.getquill.ast.Ast
import io.getquill.ast.Ident
import io.getquill.quat.Quat
import io.getquill.ast.CaseClass
import io.getquill.ast.Entity
import io.getquill.ast.Renameable


object BooSerializerSpec {

  def repickle(ast: Ast) =
    BooSerializer.Ast.deserialize(BooSerializer.Ast.serialize(ast))

  def repickle(quat: Quat) =
    BooSerializer.Quat.deserialize(BooSerializer.Quat.serialize(quat))

  def caseClass() = {
    val v = CaseClass(List("foo" -> Ident("one", Quat.Value), "bar" -> Ident("two", Quat.Value)))
    println(repickle(v) == v)
  }

  def entity() = {
    val v = new Entity("Person", List())(Quat.Product("name" -> Quat.Value, "age" -> Quat.Value))(Renameable.neutral)
    println(repickle(v) == v)
  }

  def productQuat() = {
    val v = Quat.Product("name" -> Quat.Value, "age" -> Quat.Value)
    println(repickle(v) == v)
  }



  def main(args: Array[String]): Unit = {
    caseClass()
    entity()
    productQuat()
  }
}
