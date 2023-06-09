package io.getquill.ported

import io.getquill.Spec
import io.getquill.testContext
import io.getquill.testContext._
import io.getquill._
import io.getquill.context.mirror.Row
import io.getquill.idiom.Idiom
import io.getquill.context.ExecutionType

case class ValueClass(value: Int) extends AnyVal
case class GenericValueClass[T](value: T) extends AnyVal

class ContextMacroSpec extends Spec {
  case class Person(name: String, age: Int)

  "runs actions" - {
    "non-parametrized" - {
      "action" in {
        inline def q = quote {
          qr1.delete
        }
        testContext.run(q).triple mustEqual ("""querySchema("TestEntity").delete""", List(), ExecutionType.Static)
        testContext.translate(q) mustEqual """querySchema("TestEntity").delete"""
      }
      "action returning" in {
        val p = Person("Joe", 123)
        inline def q = quote {
          query[Person].insertValue(lift(p)).returning(_.age)
        }
        testContext.run(q).triple mustEqual (("""querySchema("Person").insert(_$V => _$V.name -> ?, _$V => _$V.age -> ?).returning((x1) => x1.age)""", List("Joe", 123), ExecutionType.Static))
        testContext.translate(q) mustEqual """querySchema("Person").insert(_$V => _$V.name -> 'Joe', _$V => _$V.age -> 123).returning((x1) => x1.age)"""
      }
      "single" in {
        inline def q = quote("123")
        testContext.run(q).triple mustEqual (""""123".map(x => x)""", List(), ExecutionType.Static)
        testContext.translate(q) mustEqual """"123".map(x => x)"""
      }
      "batch" in {
        inline def q = quote {
          liftQuery(List(Person("Joe", 123))).foreach(e => query[Person].insertValue(e))
        }
        testContext.run(q).triple mustEqual ("""querySchema("Person").insert(_$V => _$V.name -> ?, _$V => _$V.age -> ?)""", List(List("Joe", 123)), ExecutionType.Static)
        testContext.translate(q) mustEqual List("""querySchema("Person").insert(_$V => _$V.name -> 'Joe', _$V => _$V.age -> 123)""")
      }
      "batch returning" in {
        inline def q = quote {
          liftQuery(List(Person("Joe", 123))).foreach(e => query[Person].insertValue(e).returning(_.age))
        }
        testContext.run(q).triple mustEqual ("""querySchema("Person").insert(_$V => _$V.name -> ?, _$V => _$V.age -> ?).returning((x2) => x2.age)""", List(List("Joe", 123)), ExecutionType.Static)
        testContext.translate(q) mustEqual List("""querySchema("Person").insert(_$V => _$V.name -> 'Joe', _$V => _$V.age -> 123).returning((x2) => x2.age)""")
      }
      "sql" in {
        inline def q = quote {
          sql"STRING".as[Action[TestEntity]]
        }
        testContext.run(q).triple mustEqual
          ("""sql"STRING"""", List(), ExecutionType.Static)
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.delete
      //   }
      //   testContext.run(q.dynamic).string mustEqual
      //     """querySchema("TestEntity").delete"""
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta] = quote(query[T].delete)
      //   val r = testContext.run(test[TestEntity])
      //   r.string mustEqual """querySchema("TestEntity").delete"""
      // }
    }
    "parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("a")).delete
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?).delete"""
        r.prepareRow mustEqual Row("_1" -> "a")
        r.info.executionType mustEqual ExecutionType.Static
      }
      "sql" in {
        inline def q = quote {
          sql"t = ${lift("a")}".as[Action[TestEntity]]
        }
        val r = testContext.run(q)
        r.string mustEqual s"""sql"t = $${?}""""
        r.prepareRow mustEqual Row("_1" -> "a")
        r.info.executionType mustEqual ExecutionType.Static
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     sql"t = ${lift("a")}".as[Action[TestEntity]]
      //   }
      //   val r = testContext.run(q.dynamic)
      //   r.string mustEqual s"""sql"t = $${?}""""
      //   r.prepareRow mustEqual Row("_1" -> "a")
      // }
      // "dynamic type param" in {
      //   import language.reflectiveCalls
      //   def test[T <: { def i: Int }: SchemaMeta] = quote {
      //     query[T].filter(t => t.i == lift(1)).delete
      //   }
      //   val r = testContext.run(test[TestEntity])
      //   r.string mustEqual """querySchema("TestEntity").filter(t => t.i == ?).delete"""
      //   r.prepareRow mustEqual Row("_1" -> 1)
      // }
    }
  }

  "translate actions" - {
    "non-parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.delete
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").delete"""
      }
      "sql" in {
        inline def q = quote {
          sql"STRING".as[Action[TestEntity]]
        }
        testContext.translate(q) mustEqual
          """sql"STRING""""
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.delete
      //   }
      //   testContext.translate(q.dynamic) mustEqual
      //     """querySchema("TestEntity").delete"""
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta] = quote(query[T].delete)
      //   testContext.translate(test[TestEntity]) mustEqual
      //     """querySchema("TestEntity").delete"""
      // }
    }
    "parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("a")).delete
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").filter(t => t.s == 'a').delete"""
      }
      "sql" in {
        inline def q = quote {
          sql"t = ${lift("a")}".as[Action[TestEntity]]
        }
        testContext.translate(q) mustEqual s"""sql"t = $${'a'}""""
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     sql"t = ${lift("a")}".as[Action[TestEntity]]
      //   }
      //   testContext.translate(q.dynamic) mustEqual s"""sql"t = $${'a'}""""
      // }
      // "dynamic type param" in {
      //   import language.reflectiveCalls
      //   def test[T <: { def i: Int }: SchemaMeta] = quote {
      //     query[T].filter(t => t.i == lift(1)).delete
      //   }
      //   testContext.translate(test[TestEntity]) mustEqual
      //     """querySchema("TestEntity").filter(t => t.i == 1).delete"""
      // }
    }
  }

  "runs queries" - {
    "non-parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.map(t => t.s)
        }
        testContext.run(q).string mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "sql" in {
        inline def q = quote {
          sql"STRING".as[Query[TestEntity]].map(t => t.s)
        }
        testContext.run(q).string mustEqual
          """sql"STRING".map(t => t.s)"""
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.map(t => t.s)
      //   }
      //   testContext.run(q.dynamic).string mustEqual
      //     """querySchema("TestEntity").map(t => t.s)"""
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta] = quote(query[T])
      //   val r = testContext.run(test[TestEntity])
      //   r.string mustEqual """querySchema("TestEntity")"""
      // }
    }
    "parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?)"""
        r.prepareRow mustEqual Row("_1" -> "a")
        r.info.executionType mustEqual ExecutionType.Static
      }

      "value class" in {
        case class Entity(x: ValueClass)
        inline def q = quote {
          query[Entity].filter(t => t.x == lift(ValueClass(1)))
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("Entity").filter(t => t.x == ?)"""
        r.prepareRow mustEqual Row("_1" -> 1)
        r.info.executionType mustEqual ExecutionType.Static
      }
      // Generic value classes not supported in protoquill yet
      // "generic value class" in {
      //   case class Entity(x: GenericValueClass[Int])
      //   inline def q = quote {
      //     query[Entity].filter(t => t.x == lift(GenericValueClass(1)))
      //   }
      //   val r = testContext.run(q)
      //   r.string mustEqual """querySchema("Entity").filter(t => t.x == ?)"""
      //   r.prepareRow mustEqual Row("_1" -> 1)
      // }
      "sql" in {
        inline def q = quote {
          sql"SELECT ${lift("a")}".as[Query[String]]
        }
        val r = testContext.run(q)
        r.string mustEqual s"""sql"SELECT $${?}".map(x => x)"""
        r.prepareRow mustEqual Row("_1" -> "a")
        r.info.executionType mustEqual ExecutionType.Static
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.filter(t => t.s == lift("a"))
      //   }
      //   val r = testContext.run(q.dynamic)
      //   r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?)"""
      //   r.prepareRow mustEqual Row("_1" -> "a")
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta: QueryMeta] = quote {
      //     query[T].map(t => lift(1))
      //   }
      //   val r = testContext.run(test[TestEntity])
      //   r.string mustEqual """querySchema("TestEntity").map(t => ?)"""
      //   r.prepareRow mustEqual Row("_1" -> 1)
      // }
    }
    "aggregated" in {
      inline def q = quote {
        qr1.map(t => t.i).max
      }
      val r = testContext.run(q)
      r.string mustEqual """querySchema("TestEntity").map(t => t.i).max"""
      r.info.executionType mustEqual ExecutionType.Static
    }
  }

  "translate queries" - {
    "non-parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.map(t => t.s)
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "sql" in {
        inline def q = quote {
          sql"STRING".as[Query[TestEntity]].map(t => t.s)
        }
        testContext.translate(q) mustEqual
          """sql"STRING".map(t => t.s)"""
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.map(t => t.s)
      //   }
      //   testContext.translate(q.dynamic) mustEqual
      //     """querySchema("TestEntity").map(t => t.s)"""
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta] = quote(query[T])
      //   testContext.translate(test[TestEntity]) mustEqual
      //     """querySchema("TestEntity")"""
      // }
    }
    "parametrized" - {
      "normal" in {
        inline def q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").filter(t => t.s == 'a')"""
      }

      "value class" in {
        case class Entity(x: ValueClass)
        inline def q = quote {
          query[Entity].filter(t => t.x == lift(ValueClass(1)))
        }
        testContext.translate(q) mustEqual
          """querySchema("Entity").filter(t => t.x == 1)"""
      }
      // Generic value classes not supported by protoquill yet
      // "generic value class" in {
      //   case class Entity(x: GenericValueClass[Int])
      //   inline def q = quote {
      //     query[Entity].filter(t => t.x == lift(GenericValueClass(1)))
      //   }
      //   testContext.translate(q) mustEqual
      //     """querySchema("Entity").filter(t => t.x == 1)"""
      // }
      "sql" in {
        inline def q = quote {
          sql"SELECT ${lift("a")}".as[Query[String]]
        }
        testContext.translate(q) mustEqual s"""sql"SELECT $${'a'}".map(x => x)"""
      }
      // Dynamic not supported in protoquill yet
      // "dynamic" in {
      //   inline def q = quote {
      //     qr1.filter(t => t.s == lift("a"))
      //   }
      //   testContext.translate(q.dynamic) mustEqual
      //     """querySchema("TestEntity").filter(t => t.s == 'a')"""
      // }
      // "dynamic type param" in {
      //   def test[T: SchemaMeta: QueryMeta] = quote {
      //     query[T].map(t => lift(1))
      //   }
      //   testContext.translate(test[TestEntity]) mustEqual
      //     """querySchema("TestEntity").map(t => 1)"""
      // }
    }
    "aggregated" in {
      inline def q = quote {
        qr1.map(t => t.i).max
      }
      testContext.translate(q) mustEqual
        """querySchema("TestEntity").map(t => t.i).max"""
    }
  }

  "fails if there's a free variable" in {
    val i = 1
    inline def q =
      quote {
        qr1.filter(_.i == i)
      }
    "testContext.run(q)" mustNot compile
    "testContext.translate(q)" mustNot compile
  }

  "falls back to dynamic queries if idiom/naming are not known" in {
    import language.existentials
    def test(ctx: MirrorContext[_ <: Idiom, _ <: NamingStrategy]) = {
      import ctx._
      ctx.run(query[TestEntity])
    }

    def translateTest(ctx: MirrorContext[_ <: Idiom, _ <: NamingStrategy]) = {
      import ctx._
      ctx.translate(query[TestEntity])
    }

    test(testContext).triple mustEqual ("""querySchema("TestEntity")""", List(), ExecutionType.Dynamic)
    translateTest(testContext) mustEqual """querySchema("TestEntity")"""
  }

  "supports composite naming strategies" - {
    "two" in {
      object ctx extends MirrorContext(MirrorIdiom, NamingStrategy(Literal, Escape)) with TestEntities
      import ctx._
      ctx.run(query[TestEntity]).string mustEqual """querySchema("TestEntity")"""
      ctx.translate(query[TestEntity]) mustEqual """querySchema("TestEntity")"""
    }
    "three" in {
      object ctx extends MirrorContext(MirrorIdiom, NamingStrategy(Literal, Escape, UpperCase)) with TestEntities
      import ctx._
      ctx.run(query[TestEntity]).string mustEqual """querySchema("TestEntity")"""
      ctx.translate(query[TestEntity]) mustEqual """querySchema("TestEntity")"""
    }
    "four" in {
      object ctx extends MirrorContext(MirrorIdiom, NamingStrategy(Literal, Escape, UpperCase, SnakeCase)) with TestEntities
      import ctx._
      ctx.run(query[TestEntity]).string mustEqual """querySchema("TestEntity")"""
      ctx.translate(query[TestEntity]) mustEqual """querySchema("TestEntity")"""
    }
  }
}
