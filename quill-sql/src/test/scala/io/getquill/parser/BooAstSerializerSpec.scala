package io.getquill.parser

import io.getquill.quat.Quat
import io.getquill.ast._
import io.getquill.Ord
import io.getquill.ast.PropertyOrdering
import io.getquill.PicklingHelper._
import io.getquill.Spec

class BooSerializerSpec extends Spec {

  "caseClass" in {
    val v = CaseClass("Prod", List("foo" -> Ident("one", Quat.Value), "bar" -> Ident("two", Quat.Value)))
    assert(repickle(v) == v)
  }

  "entity" in {
    val v = new Entity("Person", List())(Quat.Product("Prod", "name" -> Quat.Value, "age" -> Quat.Value))(Renameable.neutral)
    assert(repickle(v) == v)
  }

  "productQuat" in {
    val v = Quat.Product("Prod", "name" -> Quat.Value, "age" -> Quat.Value)
    assert(repickle(v) == v)
  }

  "constant" in {
    assert(repickle(Constant.auto("s")) == Constant.auto("s"))
    assert(repickle(Constant.auto(123)) == Constant.auto(123))
    assert(repickle(Constant.auto(123L)) == Constant.auto(123L))
    assert(repickle(Constant.auto(123.toShort)) == Constant.auto(123.toShort))
    val f = 123.toFloat
    assert(repickle(Constant.auto(f)) == Constant.auto(f))
    val d = 123.toDouble
    assert(repickle(Constant.auto(d)) == Constant.auto(d))
    assert(repickle(Constant.auto(true)) == Constant.auto(true))
    assert(repickle(Constant.auto(())) == Constant.auto(()))
  }

  "insert" in {
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("Prod", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
    val v = Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s"))))
    assert(repickle(v) == v)
  }

  "onConflict" in {
    import io.getquill.ast._
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("Prod", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
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
    assert(repickle(v) == v)
  }

  "tupleProperty" in {
    val QV = Quat.Value
    // val v = Tuple(List(Property(Ident("person", Quat.Product("name" -> QV, "age" -> QV)), "name")))
    val v = Tuple(List(Ident("person", Quat.Value)))
    assert(repickle(v) == v)
  }

  "binaryAndUnaryOp" in {
    val QV = Quat.Value
    val PV = Quat.Product("Prod", "name" -> Quat.Value, "age" -> Quat.Value)
    val v =
      BinaryOperation(
        Property(Ident("person", PV), "age"),
        EqualityOperator.`_==`,
        UnaryOperation(
          NumericOperator.`-`,
          Constant.auto(123)
        )
      )
    assert(repickle(v) == v)
  }

  "sortByWithEntity" in {
    val QV = Quat.Value
    val QBV = Quat.BooleanValue
    val TestEntityQuat = Quat.Product("Prod", "s" -> QV, "i" -> QV, "l" -> QV, "o" -> QV, "b" -> QBV)
    def IdT(name: String) = Ident(name, TestEntityQuat)
    def PropT(id: String, prop: String) = Property(Ident(id, TestEntityQuat), prop)

    val v =
      SortBy(
        Entity("TestEntity", Nil, TestEntityQuat),
        IdT("t"),
        Tuple(List(PropT("t", "i"), PropT("t", "s"))),
        TupleOrdering(List(Desc, Asc))
      )
    assert(repickle(v) == v)
  }
}
