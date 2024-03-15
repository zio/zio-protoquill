package io.getquill.context.sql.idiom

import io.getquill._

class SqlIdiomOptionCompareSpec extends Spec {

  case class TwoIntsClassScope(one: Int, two: Int)

  // remove the === matcher from scalatest so that we can test === in Context.extra
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

  "strictly checks non-ansi option operation" - {
    import io.getquill.context.sql.nonAnsiTestContext._
    import io.getquill.context.sql.{ nonAnsiTestContext => testContext }

    "Option == Option(constant)" in {
      inline def q = quote {
        qr1.filter(t => t.o == Option(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND t.o = 1"
    }
    "Option(constant) == Option" in {
      inline def q = quote {
        qr1.filter(t => Option(1) == t.o)
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND 1 = t.o"
    }
    "Option != Option(constant)" in {
      inline def q = quote {
        qr1.filter(t => t.o != Option(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NULL OR t.o <> 1"
    }
    "Option(constant) != Option" in {
      inline def q = quote {
        qr1.filter(t => Option(1) != t.o)
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NULL OR 1 <> t.o"
    }
    "Database-level === and =!= operators" - {
      import extras._

      "Option === Option" in {
        inline def q = quote {
          qr1.filter(t => t.o === t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND t.o IS NOT NULL AND t.o = t.o"
      }
      "Option === Option(constant)" in {
        inline def q = quote {
          qr1.filter(t => t.o === Option(1))
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND t.o = 1"
      }
      "Option(constant) === Option" in {
        inline def q = quote {
          qr1.filter(t => Option(1) === t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND 1 = t.o"
      }

      "Option =!= Option" in {
        inline def q = quote {
          qr1.filter(t => t.o =!= t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND t.o IS NOT NULL AND t.o <> t.o"
      }
      "Option =!= Option(constant)" in {
        inline def q = quote {
          qr1.filter(t => t.o =!= Option(1))
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND t.o <> 1"
      }
      "Option(constant) =!= Option" in {
        inline def q = quote {
          qr1.filter(t => Option(1) =!= t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NOT NULL AND 1 <> t.o"
      }
    }
    "contains" in {
      inline def q = quote {
        qr1.filter(t => t.o.contains(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o = 1"
    }
    "exists" in {
      inline def q = quote {
        qr1.filter(t => t.o.exists(op => op != 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o <> 1"
    }
    "exists with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.o.exists(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL"
    }
    "forall" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.forall(op => op == 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (t.o = 1 OR t.o IS NULL)"
    }
    "forall with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.forall(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL OR t.o IS NULL)"
    }
    "filterIfDefined" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.filterIfDefined(op => op == 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (t.o = 1 OR t.o IS NULL)"
    }
    "filter IfDefined with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.filterIfDefined(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL OR t.o IS NULL)"
    }
    "embedded" - {
      case class TestEntity(optionalEmbedded: Option[EmbeddedEntity])
      case class EmbeddedEntity(value: Int) extends Embedded

      "exists" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.value == 1))
        }

        testContext.run(q).string mustEqual
          "SELECT t.value FROM TestEntity t WHERE t.value = 1"
      }
      // TODO These should fail in the parser should look into throwing an error for them
      // "forall" in {
      //   "quote(query[TestEntity].filter(t => t.optionalEmbedded.forall(_.value == 1)))" mustNot compile
      // }
      // "filterIfDefined" in {
      //   "quote(query[TestEntity].filter(t => t.optionalEmbedded.filterIfDefined(_.value == 1)))" mustNot compile
      // }
    }
    "nested" - {
      case class TestEntity(optionalEmbedded: Option[EmbeddedEntity])
      case class EmbeddedEntity(optionalValue: Option[Int]) extends Embedded

      "contains" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.contains(1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1"
      }
      "exists" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.exists(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1"
      }
      "exists with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.exists(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL"
      }
      "forall" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.forall(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1 OR t.optionalValue IS NULL"
      }
      "forall with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.forall(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL OR t.optionalValue IS NULL"
      }
      "filterIfDefined" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.filterIfDefined(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1 OR t.optionalValue IS NULL"
      }
      "filterIfDefined with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.filterIfDefined(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL OR t.optionalValue IS NULL"
      }
    }
  }

  "optimizes checks for ansi option operation" - {
    import io.getquill.context.sql.testContext
    import io.getquill.context.sql.testContext._

    "Option == Option(constant)" in {
      inline def q = quote {
        qr1.filter(t => t.o == Option(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o = 1"
    }
    "Option(constant) == Option" in {
      inline def q = quote {
        qr1.filter(t => Option(1) == t.o)
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE 1 = t.o"
    }
    "Option != Option(constant)" in {
      inline def q = quote {
        qr1.filter(t => t.o != Option(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NULL OR t.o <> 1"
    }
    "Option(constant) != Option" in {
      inline def q = quote {
        qr1.filter(t => Option(1) != t.o)
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o IS NULL OR 1 <> t.o"
    }
    "Database-level === and =!= operators" - {
      import extras._

      "Option === Option" in {
        inline def q = quote {
          qr1.filter(t => t.o === t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o = t.o"
      }
      "Option === Option(constant)" in {
        inline def q = quote {
          qr1.filter(t => t.o === Option(1))
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o = 1"
      }
      "Option(constant) === Option" in {
        inline def q = quote {
          qr1.filter(t => Option(1) === t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE 1 = t.o"
      }

      "Option =!= Option" in {
        inline def q = quote {
          qr1.filter(t => t.o =!= t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o <> t.o"
      }
      "Option =!= Option(constant)" in {
        inline def q = quote {
          qr1.filter(t => t.o =!= Option(1))
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o <> 1"
      }
      "Option(constant) =!= Option" in {
        inline def q = quote {
          qr1.filter(t => Option(1) =!= t.o)
        }
        testContext.run(q).string mustEqual
          "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE 1 <> t.o"
      }
    }
    "contains" in {
      inline def q = quote {
        qr1.filter(t => t.o.contains(1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o = 1"
    }
    "exists" in {
      inline def q = quote {
        qr1.filter(t => t.o.exists(op => op != 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.o <> 1"
    }
    "exists with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.o.exists(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL"
    }
    "forall" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.forall(op => op == 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (t.o = 1 OR t.o IS NULL)"
    }
    "forall with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.forall(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL OR t.o IS NULL)"
    }
    "filterIfDefined" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.filterIfDefined(op => op == 1))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (t.o = 1 OR t.o IS NULL)"
    }
    "filterIfDefined with null-check" in {
      inline def q = quote {
        qr1.filter(t => t.i != 1 && t.o.filterIfDefined(op => if (op != 1) false else true))
      }
      testContext.run(q).string mustEqual
        "SELECT t.s, t.i, t.l, t.o, t.b FROM TestEntity t WHERE t.i <> 1 AND (CASE WHEN t.o <> 1 THEN false ELSE true END AND t.o IS NOT NULL OR t.o IS NULL)"
    }
    "embedded" - {
      case class TestEntity(optionalEmbedded: Option[EmbeddedEntity])
      case class EmbeddedEntity(value: Int) extends Embedded

      "exists" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.value == 1))
        }

        testContext.run(q).string mustEqual
          "SELECT t.value FROM TestEntity t WHERE t.value = 1"
      }
      // TODO Should look into making these cases fail in the parser
      // "forall" in {
      //   "quote(query[TestEntity].filter(t => t.optionalEmbedded.forall(_.value == 1)))" mustNot compile
      // }
      // "filterIfDefined" in {
      //   "quote(query[TestEntity].filter(t => t.optionalEmbedded.filterIfDefined(_.value == 1)))" mustNot compile
      // }
    }
    "nested" - {
      case class TestEntity(optionalEmbedded: Option[EmbeddedEntity])
      case class EmbeddedEntity(optionalValue: Option[Int]) extends Embedded

      "contains" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.contains(1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1"
      }
      "exists" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.exists(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1"
      }
      "exists with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.exists(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL"
      }
      "forall" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.forall(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1 OR t.optionalValue IS NULL"
      }
      "forall with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.forall(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL OR t.optionalValue IS NULL"
      }
      "filterIfDefined" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.filterIfDefined(_ == 1)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE t.optionalValue = 1 OR t.optionalValue IS NULL"
      }
      "filterIfDefined with null-check" in {
        inline def q = quote {
          query[TestEntity].filter(t => t.optionalEmbedded.exists(_.optionalValue.filterIfDefined(v => if (v == 1) true else false)))
        }

        testContext.run(q).string mustEqual
          "SELECT t.optionalValue FROM TestEntity t WHERE CASE WHEN t.optionalValue = 1 THEN true ELSE false END AND t.optionalValue IS NOT NULL OR t.optionalValue IS NULL"
      }
    }
  }
}
