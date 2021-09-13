package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.Spec
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.quat.Quat
import io.getquill.quat.quatOf
import io.getquill._
import org.scalatest.Inside

class ActionTest extends Spec with NonSerializingQuotation with TestEntities with Inside {
  case class ActionTestEntity(id: Int)

  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  def internalizeVLabel(ast: Ast) =
    NameChangeIdent{ case "v" => "_$V" }.apply(ast)

  "action" - {
    "update" - {
      "field" in {
        inline def q = quote {
          qr1.update(t => t.s -> "s")
        }
        quote(unquote(q)).ast mustEqual Update(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s"))))
      }
      "set field using another field" in {
        inline def q = quote {
          qr1.update(t => t.i -> (t.i + 1))
        }
        quote(unquote(q)).ast mustEqual Update(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "i"), BinaryOperation(Property(Ident("t"), "i"), NumericOperator.`+`, Constant.auto(1)))))
      }
      "case class" in {
        inline def q = quote {
          (t: TestEntity) => qr1.update(t)
        }
        val n = quote {
          (t: TestEntity) =>
            qr1.update(
              v => v.s -> t.s,
              v => v.i -> t.i,
              v => v.l -> t.l,
              v => v.o -> t.o,
              v => v.b -> t.b
            )
        }

        quote(unquote(q)).ast mustEqual internalizeVLabel(n.ast)
      }
      "explicit `Predef.ArrowAssoc`" in {
        inline def q = quote {
          qr1.update(t => Predef.ArrowAssoc(t.s).->[String]("s"))
        }
        quote(unquote(q)).ast mustEqual Update(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s"))))
      }
      // TODO Does Dotty even support this?
      // "unicode arrow must compile" in {
      //    """|quote {
      //       |  qr1.filter(t => t.i == 1).update(_.s → "new", _.i → 0)
      //       |}
      //   """.stripMargin must compile
      // }
    }
    "insert" - {
      "field" in {
        inline def q = quote {
          qr1.insert(t => t.s -> "s")
        }
        quote(unquote(q)).ast mustEqual Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s"))))
      }
      "case class" in {
        inline def q = quote {
          (t: TestEntity) => qr1.insert(t)
        }
        val n = quote {
          (t: TestEntity) =>
            qr1.insert(
              v => v.s -> t.s,
              v => v.i -> t.i,
              v => v.l -> t.l,
              v => v.o -> t.o,
              v => v.b -> t.b
            )
        }
        quote(unquote(q)).ast mustEqual internalizeVLabel(n.ast)
      }
      "insert with conflict" - {
        "onConflictIgnore" in {
          inline def q = quote {
            qr1.insert(t => t.s -> "s").onConflictIgnore
          }
          quote(unquote(q)).ast mustEqual OnConflict(
            Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s")))),
            OnConflict.NoTarget,
            OnConflict.Ignore
          )
        }
        "onConflictIgnore(_.i)" in {
          inline def q = quote {
            qr1.insert(t => t.s -> "s").onConflictIgnore(r => r.i)
          }
          quote(unquote(q)).ast mustEqual OnConflict(
            Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s")))),
            OnConflict.Properties(List(Property(Ident("r"), "i"))),
            OnConflict.Ignore
          )
        }
        "onConflictUpdate((t, e) => ...)" in {
          inline def q = quote {
            qr1.insert(t => t.s -> "s").onConflictUpdate((t, e) => t.s -> e.s, (t, e) => t.l -> e.l)
          }

          def IdT(name: String) = Ident(name, TestEntityQuat)
          quote(unquote(q)).ast mustEqual OnConflict(
            Insert(Entity("TestEntity", Nil, TestEntityQuat), List(Assignment(Ident("t"), Property(Ident("t"), "s"), Constant.auto("s")))),
            OnConflict.NoTarget,
            OnConflict.Update(
              List(
                AssignmentDual(IdT("t"), IdT("e"), Property(IdT("t"), "s"), Property(OnConflict.Excluded(IdT("e")), "s")),
                AssignmentDual(IdT("t"), IdT("e"), Property(IdT("t"), "l"), Property(OnConflict.Excluded(IdT("e")), "l"))
              )
            )
          )
        }
      }
      "batch delete" in {
        val ctx = new MirrorContext(MirrorSqlDialect, Literal)
        import ctx._

        val list = List(1, 2)
        inline def delete = quote((i: Int) => qr1.filter(_.i == i).delete)
        inline def q = quote {
          liftQuery(list).foreach(i => delete(i))
        }
        inside(quote(unquote(q)).ast) {
          case Foreach(ScalarTag(_), Ident("i", quat), body) =>
            body mustEqual delete.ast.body
            quat mustEqual Quat.Value
        }
      }
      "batch insert" in {
        // liftQuery requires a context since it does lifts
        val ctx = new MirrorContext(MirrorSqlDialect, Literal)
        import ctx._

        val list = List(
          ActionTestEntity(1),
          ActionTestEntity(2)
        )
        inline def insertRow = quote((row: ActionTestEntity) => query[ActionTestEntity].insert(row))
        inline def q = quote {
          liftQuery(list).foreach(row => insertRow(row))
        }
        inside(quote(unquote(q)).ast) {
          case Foreach(ScalarTag(_), Ident("row", quat), body) =>
            body mustEqual insertRow.ast.body
            quat mustEqual quatOf[ActionTestEntity]
        }
      }
      // TODO Double Quoting is not supported yet. Need to look into how to do this
      // "batch with Quoted[Action[T]]" in {
      //   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
      //   import ctx._

      //   val list = List(
      //     ActionTestEntity(1),
      //     ActionTestEntity(2)
      //   )
      //   val insert = quote((row: ActionTestEntity) => query[ActionTestEntity].insert(row))
      //   inline def q = quote(liftQuery(list).foreach(row => quote(insert(row))))
      //   quote(unquote(q)).ast mustEqual
      //     Foreach(CaseClassQueryLift("q.list", list, quatOf[ActionTestEntity]), Ident("row"), insert.ast.body)
      // }
      // TODO Does Dotty even support this?
      // "unicode arrow must compile" in {
      //   """|quote {
      //       |  qr1.insert(_.s → "new", _.i → 0)
      //       |}
      //   """.stripMargin must compile
      // }
    }
    "delete" in {
      inline def q = quote {
        qr1.delete
      }
      quote(unquote(q)).ast mustEqual Delete(Entity("TestEntity", Nil, TestEntityQuat))
    }
    "fails if the assignment types don't match" in {
      """
        quote {
          qr1.update(t => t.i -> "s")
        }
      """ mustNot compile
    }
  }
}
