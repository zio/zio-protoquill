package io.getquill.ported.quotationspec

import scala.language.implicitConversions
import io.getquill.QuotationLot
import io.getquill.ast.{Query => AQuery, _}
import io.getquill.Quoted
import io.getquill.Planter
import io.getquill.QuotationVase
import io.getquill.QuotationLot
import org.scalatest._
import io.getquill.quat.Quat
import io.getquill.ast.Implicits._
import io.getquill.norm.NormalizeStringConcat
import io.getquill._
import io.getquill.PicklingHelper._

class OperationTest extends Spec with TestEntities with Inside {
  // remove the === matcher from scalatest so that we can test === in Context.extra
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

  extension (ast: Ast) {
    def body: Ast = ast match {
      case f: Function => f.body
      case _ => fail(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")
    }
  }

  "binary operation" - {
    "==" - {
      "normal" in {
        inline def q = quote {
          (a: Int, b: Int) => a == b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "succeeds when different numerics are used Int/Long" in {
        inline def q = quote {
          (a: Int, b: Long) => a == b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "succeeds when different numerics are used Long/Int" in {
        inline def q = quote {
          (a: Long, b: Int) => a == b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "fails if the types don't match" in {
        """
          quote {
            (a: Int, b: String) => a == b
          }
        """ mustNot compile
      }
      "comparing compatible nested types" - {
        val ia = Ident("a")
        val ib = Ident("b")

        "succeeds when Option/Option" in {
          inline def q = quote {
            (a: Option[Int], b: Option[Int]) => a == b
          }

          quote(unquote(q)).ast.body mustEqual (OptionIsEmpty(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib))
        }
        "succeeds when Option/T" in {
          """
          inline def q = quote {
            (a: Option[Int], b: Int) => a == b
          }
          """ mustNot compile
        }
        "succeeds when T/Option" in {
          """
          inline def q = quote {
            (a: Int, b: Option[Int]) => a == b
          }
          """ mustNot compile
        }
        "fails with multiple nesting T/Option[Option]" in {
          """
          inline def q = quote {
            (a: Int, b: Option[Option[Int]]) => a == b
          }
          """ mustNot compile
        }
        "succeeds with multiple nesting Option[Option]/T" in {
          """
          inline def q = quote {
            (a: Option[Option[Int]], b: Int) => a == b
          }
          """ mustNot compile
        }
        "succeeds when Option/None" in {
          """
          inline def q = quote {
            (a: Int) => a == None
          }
          """ mustNot compile
        }
        "fails when None/Option (left hand bias)" in {
          """
          inline def q = quote {
            (a: Int) => None == a
          }
          """ mustNot compile
        }
        "comparing types with suclassing" - {
          case class Foo(id: Int)
          trait Foot
          case class Bar(id: Int)
          trait Bart

          "succeeds when Option[T]/Option[T]" in {
            inline def q = quote {
              (a: Option[Foo], b: Option[Foo]) => a == b
            }
            val o = (OptionIsEmpty(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib))
            quote(unquote(q)).ast.body mustEqual o
            repickle(o) mustEqual o
          }
          "succeeds when Option[T]/Option[subclass T]" in {
            inline def q = quote {
              (a: Option[Foo], b: Option[Foo with Foot]) => a == b
            }
            val o = (OptionIsEmpty(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib))
            quote(unquote(q)).ast.body mustEqual o
            repickle(o) mustEqual o
          }
          "succeeds when Option[subclass T]/Option[T]" in {
            inline def q = quote {
              (a: Option[Foo with Foot], b: Option[Foo]) => a == b
            }
            val o = (OptionIsEmpty(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib))
            quote(unquote(q)).ast.body mustEqual o
            repickle(o) mustEqual o
          }
          "fails when Option[T]/Option[A]" in {
            """
              quote {
                (a: Option[Foo], b: Option[Bar]) => a == b
              }
            """ mustNot compile
          }
          "fails when Option[subclass1 T]/Option[subclass 2T]" in {
            """
              quote {
                (a: Option[Foo with Foot], b: Option[Foo with Bart]) => a == b
              }
            """ mustNot compile
          }
        }
      }

      "extras" - {
        import extras._
        val ia = Ident("a")
        val ib = Ident("b")

        "normal" in {
          inline def q = quote {
            (a: Int, b: Int) => a === b
          }
          quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        }
        "normal - string" in {
          inline def q = quote {
            (a: String, b: String) => a === b
          }
          quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        }
        "succeeds when different numerics are used Int/Long" in {
          inline def q = quote {
            (a: Int, b: Long) => a === b
          }
          quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        }
        "succeeds when Option/Option" in {
          inline def q = quote {
            (a: Option[Int], b: Option[Int]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
        "succeeds when Option/T" in {
          inline def q = quote {
            (a: Option[Int], b: Int) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ (ia +==+ ib)
        }
        "succeeds when T/Option" in {
          inline def q = quote {
            (a: Int, b: Option[Int]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
        "succeeds when Option/Option - Different Numerics" in {
          inline def q = quote {
            (a: Option[Int], b: Option[Long]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
        "succeeds when Option/T - Different Numerics" in {
          inline def q = quote {
            (a: Option[Int], b: Long) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ (ia +==+ ib)
        }
        "succeeds when T/Option - Different Numerics" in {
          inline def q = quote {
            (a: Int, b: Option[Long]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
        "succeeds when Option/Option - String" in {
          inline def q = quote {
            (a: Option[String], b: Option[String]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
        "succeeds when Option/T - String" in {
          inline def q = quote {
            (a: Option[String], b: String) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ (ia +==+ ib)
        }
        "succeeds when T/Option - String" in {
          inline def q = quote {
            (a: String, b: Option[String]) => a === b
          }
          quote(unquote(q)).ast.body mustEqual OptionIsDefined(ib) +&&+ (ia +==+ ib)
        }
      }
    }

    "equals" - {
      "equals method" in {
        inline def q = quote {
          (a: Int, b: Int) => a.equals(b)
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "==" in {
        inline def q = quote {
          (a: Int, b: Int) => a == b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }

      case class Foo(id: Int)
      trait Foot
      case class Bar(id: Int)
      trait Bart

      "should succeed if right is subclass" in {
        inline def q = quote {
          (a: Foo, b: Foo with Foot) => a.equals(b)
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "should succeed if left is subclass" in {
        inline def q = quote {
          (a: Foo with Foot, b: Foo) => a.equals(b)
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "should succeed with refinements" in {
        inline def q = quote {
          (a: Foo with Foot, b: Foo with Foot) => a.equals(b)
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_==`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "should fail if both are subclasses" in {
        "quote{ (a: Foo with Foot, b: Foo with Bart) => a.equals(b) }.ast.body" mustNot compile
      }
      "should fail if classes unrelated" in {
        "quote{ (a: Foo, b: Bar) => a.equals(b) }.ast.body" mustNot compile
      }
    }
    "!=" - {
      "normal" in {
        inline def q = quote {
          (a: Int, b: Int) => a != b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_!=`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "succeeds when different numerics are used Int/Long" in {
        inline def q = quote {
          (a: Int, b: Long) => a != b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_!=`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "succeeds when different numerics are used Long/Int" in {
        inline def q = quote {
          (a: Long, b: Int) => a != b
        }
        val b = BinaryOperation(Ident("a"), EqualityOperator.`_!=`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "fails if the types don't match" in {
        """
          quote {
            (a: Int, b: String) => a != b
          }
        """ mustNot compile
      }
      "comparing compatible nested types" - {
        val ia = Ident("a")
        val ib = Ident("b")

        "succeeds when Option/Option" in {
          inline def q = quote {
            (a: Option[Int], b: Option[Int]) => a != b
          }
          val o = (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib)
          quote(unquote(q)).ast.body mustEqual o
          repickle(o) mustEqual o
        }
        "fails when Option/T" in {
          """
          inline def q = quote {
            (a: Option[Int], b: Int) => a != b
          }
          """ mustNot compile
        }
        "fails when T/Option" in {
          """
          inline def q = quote {
            (a: Int, b: Option[Int]) => a != b
          }
          """ mustNot compile
        }
        "fails with multiple nesting T/Option[Option]" in {
          """
          inline def q = quote {
            (a: Int, b: Option[Option[Int]]) => a != b
          }
          """ mustNot compile
        }
        "fails with multiple nesting Option[Option]/T" in {
          """
          inline def q = quote {
            (a: Option[Option[Int]], b: Int) => a != b
          }
          """ mustNot compile
        }
        "succeeds when Option/None" in {
          """
          inline def q = quote {
            (a: Int) => a != None
          }
          """ mustNot compile
        }
        "fails when None/Option (left hand bias)" in {
          """
          inline def q = quote {
            (a: Int) => None != a
          }
          """ mustNot compile
        }
        "comparing types with suclassing" - {
          case class Foo(id: Int)
          trait Foot
          case class Bar(id: Int)
          trait Bart

          "succeeds when Option[T]/Option[subclass T]" in {
            inline def q = quote {
              (a: Option[Foo], b: Option[Foo with Foot]) => a != b
            }
            val o = (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib)
            quote(unquote(q)).ast.body mustEqual o
            repickle(o) mustEqual o
          }
          "succeeds when Option[subclass T]/Option[T]" in {
            inline def q = quote {
              (a: Option[Foo with Foot], b: Option[Foo]) => a != b
            }
            val o = (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib))
            quote(unquote(q)).ast.body mustEqual o
            repickle(o) mustEqual o
          }
          "fails when Option[T]/Option[A]" in {
            """
              quote {
                (a: Option[Foo], b: Option[Bar]) => a != b
              }
            """ mustNot compile
          }
          "fails when Option[subclass1 T]/Option[subclass 2T]" in {
            """
              quote {
                (a: Option[Foo with Foot], b: Option[Foo with Bart]) => a != b
              }
            """ mustNot compile
          }
        }
      }

      // do we have extras dsl yet?
      // "extras" - {
      //   import extras._
      //   val ia = Ident("a")
      //   val ib = Ident("b")

      //   "normal" in {
      //     inline def q = quote {
      //       (a: Int, b: Int) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`_!=`, Ident("b"))
      //   }
      //   "succeeds when different numerics are used Int/Long" in {
      //     inline def q = quote {
      //       (a: Int, b: Long) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`_!=`, Ident("b"))
      //   }
      //   "succeeds when Option/Option" in {
      //     inline def q = quote {
      //       (a: Option[Int], b: Option[Int]) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +!=+ ib)
      //   }
      //   "succeeds when Option/T" in {
      //     inline def q = quote {
      //       (a: Option[Int], b: Int) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ (ia +!=+ ib)
      //   }
      //   "succeeds when T/Option" in {
      //     inline def q = quote {
      //       (a: Int, b: Option[Int]) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ib) +&&+ (ia +!=+ ib)
      //   }
      //   "succeeds when Option/Option - String" in {
      //     inline def q = quote {
      //       (a: Option[String], b: Option[String]) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ OptionIsDefined(ib) +&&+ (ia +!=+ ib)
      //   }
      //   "succeeds when Option/T - String" in {
      //     inline def q = quote {
      //       (a: Option[String], b: String) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ia) +&&+ (ia +!=+ ib)
      //   }
      //   "succeeds when T/Option - String" in {
      //     inline def q = quote {
      //       (a: String, b: Option[String]) => a =!= b
      //     }
      //     quote(unquote(q)).ast.body mustEqual OptionIsDefined(ib) +&&+ (ia +!=+ ib)
      //   }
      // }
    }
    "&&" in {
      inline def q = quote {
        (a: Boolean, b: Boolean) => a && b
      }
      val b = BinaryOperation(Ident("a"), BooleanOperator.`&&`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "||" in {
      inline def q = quote {
        (a: Boolean, b: Boolean) => a || b
      }
      val b = BinaryOperation(Ident("a"), BooleanOperator.`||`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "-" in {
      inline def q = quote {
        (a: Int, b: Int) => a - b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`-`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "+" - {
      "numeric" in {
        inline def q = quote {
          (a: Int, b: Int) => a + b
        }
        val b = BinaryOperation(Ident("a"), NumericOperator.`+`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "string" in {
        inline def q = quote {
          (a: String, b: String) => a + b
        }
        val b = BinaryOperation(Ident("a"), StringOperator.`+`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "string interpolation" - {

        def normStrConcat(ast: Ast): Ast = NormalizeStringConcat(ast)

        // don't have string-context parsing yet
        // "one param" - {
        //   "end" in {
        //     inline def q = quote {
        //       (i: Int) => s"v$i"
        //     }
        //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Constant.auto("v"), StringOperator.`+`, Ident("i"))
        //   }
        //   "start" in {
        //     inline def q = quote {
        //       (i: Int) => s"${i}v"
        //     }
        //     normStrConcat(quote(unquote(q)).ast.body) mustEqual BinaryOperation(Ident("i"), StringOperator.`+`, Constant.auto("v"))
        //   }
        // }
        // "multiple params" in {
        //   inline def q = quote {
        //     (i: Int, j: Int, h: Int) => s"${i}a${j}b${h}"
        //   }
        //   normStrConcat(quote(unquote(q)).ast.body) mustEqual BinaryOperation(BinaryOperation(BinaryOperation(BinaryOperation(Ident("i"), StringOperator.`+`, Constant.auto("a")), StringOperator.`+`, Ident("j")), StringOperator.`+`, Constant.auto("b")), StringOperator.`+`, Ident("h"))
        // }
      }
    }
    "*" in {
      inline def q = quote {
        (a: Int, b: Int) => a * b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`*`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    ">" in {
      inline def q = quote {
        (a: Int, b: Int) => a > b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`>`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    ">=" in {
      inline def q = quote {
        (a: Int, b: Int) => a >= b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`>=`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "<" in {
      inline def q = quote {
        (a: Int, b: Int) => a < b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`<`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "<=" in {
      inline def q = quote {
        (a: Int, b: Int) => a <= b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`<=`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "/" in {
      inline def q = quote {
        (a: Int, b: Int) => a / b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`/`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }
    "%" in {
      inline def q = quote {
        (a: Int, b: Int) => a % b
      }
      val b = BinaryOperation(Ident("a"), NumericOperator.`%`, Ident("b"))
      quote(unquote(q)).ast.body mustEqual b
      repickle(b) mustEqual b
    }

    "contains" - {
      "query" in {
        inline def q = quote {
          (a: Query[TestEntity], b: TestEntity) =>
            a.contains(b)
        }
        val b = BinaryOperation(Ident("a"), SetOperator.`contains`, Ident("b"))
        quote(unquote(q)).ast.body mustEqual b
        repickle(b) mustEqual b
      }
      "within option operation" - {
        "forall" in {
          inline def q = quote { (a: Query[Int], b: Option[Int]) =>
            b.forall(a.contains)
          }
          quote(unquote(q)).ast.body mustBe an[OptionOperation]
          repickle(q.ast) mustEqual q.ast
        }
        "exists" in {
          inline def q = quote { (a: Query[Int], b: Option[Int]) =>
            b.exists(a.contains)
          }
          quote(unquote(q)).ast.body mustBe an[OptionOperation]
          repickle(q.ast) mustEqual q.ast
        }
        "map" in {
          inline def q = quote { (a: Query[Int], b: Option[Int]) =>
            b.map(a.contains)
          }
          quote(unquote(q)).ast.body mustBe an[OptionOperation]
          repickle(q.ast) mustEqual q.ast
        }
      }
      // "split" in {
      //   inline def q = quote {
      //     (s: String) => s.split(" ")
      //   }
      //   quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("s"), StringOperator.`split`, Constant.auto(" "))
      // }
      // "startsWith" in {
      //   inline def q = quote {
      //     (s: String) => s.startsWith(" ")
      //   }
      //   quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("s"), StringOperator.`startsWith`, Constant.auto(" "))
      // }
    }
  }

  "unary operation" - {
    "-" in {
      inline def q = quote {
        (a: Int) => -a
      }
      val v = UnaryOperation(NumericOperator.`-`, Ident("a"))
      quote(unquote(q)).ast.body mustEqual v
      repickle(v) mustEqual v
    }
    "!" in {
      inline def q = quote {
        (a: Boolean) => !a
      }
      val v = UnaryOperation(BooleanOperator.`!`, Ident("a"))
      quote(unquote(q)).ast.body mustEqual v
      repickle(v) mustEqual v
    }
    "nonEmpty" in {
      inline def q = quote {
        qr1.nonEmpty
      }
      val v = UnaryOperation(SetOperator.`nonEmpty`, Entity("TestEntity", Nil, TestEntityQuat))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "isEmpty" in {
      inline def q = quote {
        qr1.isEmpty
      }
      val v = UnaryOperation(SetOperator.`isEmpty`, Entity("TestEntity", Nil, TestEntityQuat))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "toUpperCase" in {
      inline def q = quote {
        qr1.map(t => t.s.toUpperCase)
      }
      val v = Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), UnaryOperation(StringOperator.`toUpperCase`, Property(Ident("t"), "s")))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "toLowerCase" in {
      inline def q = quote {
        qr1.map(t => t.s.toLowerCase)
      }
      val v = Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), UnaryOperation(StringOperator.`toLowerCase`, Property(Ident("t"), "s")))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "toLong" in {
      inline def q = quote {
        qr1.map(t => t.s.toLong)
      }
      val v = Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), UnaryOperation(StringOperator.`toLong`, Property(Ident("t"), "s")))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
    "toInt" in {
      inline def q = quote {
        qr1.map(t => t.s.toInt)
      }
      val v = Map(Entity("TestEntity", Nil, TestEntityQuat), Ident("t"), UnaryOperation(StringOperator.`toInt`, Property(Ident("t"), "s")))
      quote(unquote(q)).ast mustEqual v
      repickle(v) mustEqual v
    }
  }
}