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

class OperationTest extends Spec with Inside {

  extension (ast: Ast)
    def body: Ast = ast match
      case f: Function => f.body
      case _ => throw new IllegalArgumentException(s"Cannot get body from ast element: ${io.getquill.util.Messages.qprint(ast)}")

  "equals" - {
    "equals method" in {
      inline def q = quote {
        (a: Int, b: Int) => a.equals(b)
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`==`, Ident("b"))
    }
    "==" in {
      inline def q = quote {
        (a: Int, b: Int) => a == b
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`==`, Ident("b"))
    }

    case class Foo(id: Int)
    trait Foot
    case class Bar(id: Int)
    trait Bart

    "should succeed if right is subclass" in {
      inline def q = quote {
        (a: Foo, b: Foo with Foot) => a.equals(b)
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`==`, Ident("b"))
    }
    "should succeed if left is subclass" in {
      inline def q = quote {
        (a: Foo with Foot, b: Foo) => a.equals(b)
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`==`, Ident("b"))
    }
    "should succeed with refinements" in {
      inline def q = quote {
        (a: Foo with Foot, b: Foo with Foot) => a.equals(b)
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`==`, Ident("b"))
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
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`!=`, Ident("b"))
    }
    "succeeds when different numerics are used Int/Long" in {
      inline def q = quote {
        (a: Int, b: Long) => a != b
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`!=`, Ident("b"))
    }
    "succeeds when different numerics are used Long/Int" in {
      inline def q = quote {
        (a: Long, b: Int) => a != b
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`!=`, Ident("b"))
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
        quote(unquote(q)).ast.body mustEqual (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib)
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
          quote(unquote(q)).ast.body mustEqual (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib)) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib)
        }
        "succeeds when Option[subclass T]/Option[T]" in {
          inline def q = quote {
            (a: Option[Foo with Foot], b: Option[Foo]) => a != b
          }
          quote(unquote(q)).ast.body mustEqual (OptionIsDefined(ia) +&&+ OptionIsEmpty(ib) +||+ (OptionIsEmpty(ia) +&&+ OptionIsDefined(ib)) +||+ (ia +!=+ ib))
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
    //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`!=`, Ident("b"))
    //   }
    //   "succeeds when different numerics are used Int/Long" in {
    //     inline def q = quote {
    //       (a: Int, b: Long) => a =!= b
    //     }
    //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), EqualityOperator.`!=`, Ident("b"))
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
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), BooleanOperator.`&&`, Ident("b"))
  }
  "||" in {
    inline def q = quote {
      (a: Boolean, b: Boolean) => a || b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), BooleanOperator.`||`, Ident("b"))
  }
  "-" in {
    inline def q = quote {
      (a: Int, b: Int) => a - b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`-`, Ident("b"))
  }
  "+" - {
    "numeric" in {
      inline def q = quote {
        (a: Int, b: Int) => a + b
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`+`, Ident("b"))
    }
    "string" in {
      inline def q = quote {
        (a: String, b: String) => a + b
      }
      quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), StringOperator.`+`, Ident("b"))
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
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`*`, Ident("b"))
  }
  ">" in {
    inline def q = quote {
      (a: Int, b: Int) => a > b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`>`, Ident("b"))
  }
  ">=" in {
    inline def q = quote {
      (a: Int, b: Int) => a >= b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`>=`, Ident("b"))
  }
  "<" in {
    inline def q = quote {
      (a: Int, b: Int) => a < b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`<`, Ident("b"))
  }
  "<=" in {
    inline def q = quote {
      (a: Int, b: Int) => a <= b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`<=`, Ident("b"))
  }
  "/" in {
    inline def q = quote {
      (a: Int, b: Int) => a / b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`/`, Ident("b"))
  }
  "%" in {
    inline def q = quote {
      (a: Int, b: Int) => a % b
    }
    quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), NumericOperator.`%`, Ident("b"))
  }

  // don't have contains yet
  // "contains" - {
  //   "query" in {
  //     inline def q = quote {
  //       (a: Query[TestEntity], b: TestEntity) =>
  //         a.contains(b)
  //     }
  //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("a"), SetOperator.`contains`, Ident("b"))
  //   }
  //   "within option operation" - {
  //     "forall" in {
  //       inline def q = quote { (a: Query[Int], b: Option[Int]) =>
  //         b.forall(a.contains)
  //       }
  //       quote(unquote(q)).ast.body mustBe an[OptionOperation]
  //     }
  //     "exists" in {
  //       inline def q = quote { (a: Query[Int], b: Option[Int]) =>
  //         b.exists(a.contains)
  //       }
  //       quote(unquote(q)).ast.body mustBe an[OptionOperation]
  //     }
  //     "map" in {
  //       inline def q = quote { (a: Query[Int], b: Option[Int]) =>
  //         b.map(a.contains)
  //       }
  //       quote(unquote(q)).ast.body mustBe an[OptionOperation]
  //     }
  //   }
  //   "split" in {
  //     inline def q = quote {
  //       (s: String) => s.split(" ")
  //     }
  //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("s"), StringOperator.`split`, Constant.auto(" "))
  //   }
  //   "startsWith" in {
  //     inline def q = quote {
  //       (s: String) => s.startsWith(" ")
  //     }
  //     quote(unquote(q)).ast.body mustEqual BinaryOperation(Ident("s"), StringOperator.`startsWith`, Constant.auto(" "))
  //   }
  // }
}