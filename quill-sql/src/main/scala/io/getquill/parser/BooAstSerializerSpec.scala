package io.getquill.parser

import io.getquill.quat.Quat
import io.getquill.ast._
import io.getquill.Ord
import io.getquill.ast.PropertyOrdering


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

  def constant() = {
    println(repickle(Constant.auto("s")) == Constant.auto("s"))
    println(repickle(Constant.auto(123)) == Constant.auto(123))
  }

  def insert() = {
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
    val v = Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s"))))
    println(repickle(v) == v)
  }

  def onConflict() = {
    import io.getquill.ast._
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
    def IdT(name: String) = Ident(name, TestEntityQuat)
    val v =
      OnConflict(
        Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s")))),
        OnConflict.NoTarget,
        OnConflict.Update(
          List(
            AssignmentDual(IdT("t"), IdT("e"), Property(IdT("t"), "s"), Property(OnConflict.Excluded(IdT("e")), "s")),
            AssignmentDual(IdT("t"), IdT("e"), Property(IdT("t"), "l"), Property(OnConflict.Excluded(IdT("e")), "l"))
          )
        )
      )
    println(repickle(v) == v)
  }

  def tupleProperty() = {
    val QV = Quat.Value
    //val v = Tuple(List(Property(Ident("person", Quat.Product("name" -> QV, "age" -> QV)), "name")))
    val v = Tuple(List(Ident("person", Quat.Value)))
    println(repickle(v) == v)
  }

  def binaryAndUnaryOp() = {
    val QV = Quat.Value
    val PV = Quat.Product("name" -> Quat.Value, "age" -> Quat.Value)
    val v =
      BinaryOperation(
        Property(Ident("person", PV), "age"),
        EqualityOperator.`_==`,
        UnaryOperation(
          NumericOperator.`-`,
          Constant.auto(123)
        )
      )
    println(repickle(v) == v)
  }

  def sortByWithEntity() = {
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
    def IdT(name: String) = Ident(name, TestEntityQuat)
    def PropT(id: String, prop: String) = Property(Ident(id, TestEntityQuat), prop)

    val v =
      SortBy(
        Entity("TestEntity", Nil, TestEntityQuat),
        IdT("t"),
        Tuple(List(PropT("t","i"), PropT("t","s"))), TupleOrdering(List(Desc, Asc))
      )
    println(repickle(v) == v)
  }


  def main(args: Array[String]): Unit = {
    // caseClass()
    // entity()
    // productQuat()
    //constant()
    // insert()
    // onConflict()
    // tupleProperty()
    // binaryAndUnaryOp()
    sortByWithEntity()
  }
}
