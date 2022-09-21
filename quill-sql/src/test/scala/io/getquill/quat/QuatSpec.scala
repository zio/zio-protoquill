package io.getquill.quat

import io.getquill.quotation.QuatException
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest._
import io.getquill.quat._

import io.getquill._

object TestEnum:
  enum MyEnum:
    case Foo
    case Bar
  case class MyEnumContainer(e: MyEnum)
  enum ProductEnum:
    case Foo(stuff: String)
    case Bar(stuff: String, otherStuff: String)

class QuatSpec extends AnyFreeSpec {

  val ctx = new SqlMirrorContext(PostgresDialect, Literal)
  import ctx._

  "enum" - {
    // TODO these should specifically return a compile-time error:
    // The Co-Product element io.getquill.quat.TestEnum.MyEnum.Foo was not a Case Class
    // should see if there is a way to do that
    "should fail if plain enum no encoder" in {
      "quatOf[TestEnum.MyEnum]" mustNot compile
      "quote(query[TestEnum.MyEnumContainer])" mustNot compile
    }
    // TODO Why doesn't this work?
    // "should succeed plain enum if there is encoder" in {
    //   given MappedEncoding[TestEnum.MyEnum, String](_.toString)
    //   quote(query[TestEnum.MyEnumContainer])
    // }
    "should succeed product-type enum" in {
      quatOf[TestEnum.ProductEnum] mustEqual Quat.Product("stuff" -> Quat.Value, "otherStuff" -> Quat.Value)
    }
  }

  "boolean and optional boolean" in {
    case class MyPerson(name: String, isHuman: Boolean, isRussian: Option[Boolean])
    val MyPersonQuat = Quat.Product("name" -> Quat.Value, "isHuman" -> Quat.BooleanValue, "isRussian" -> Quat.BooleanValue)
    MyPersonQuat mustEqual quatOf[MyPerson]
    quote(query[MyPerson]).ast.quat mustEqual MyPersonQuat
  }

  "should support standard case class" in {
    case class MyPerson(firstName: String, lastName: String, age: Int)
    val MyPersonQuat = Quat.LeafProduct("firstName", "lastName", "age")
    MyPersonQuat mustEqual quatOf[MyPerson]
    quote(query[MyPerson]).ast.quat mustEqual MyPersonQuat
  }

  "should support embedded" in {
    case class MyName(first: String, last: String) extends Embedded
    case class MyPerson(name: MyName, age: Int) extends Embedded
    val MyPersonQuat = Quat.Product("name" -> Quat.LeafProduct("first", "last"), "age" -> Quat.Value)
    MyPersonQuat mustEqual quatOf[MyPerson]
    quote(query[MyPerson]).ast.quat mustEqual MyPersonQuat
  }

  "should support multi-level embedded" in {
    case class MyName(first: String, last: String) extends Embedded
    case class MyId(name: MyName, memberNum: Int) extends Embedded
    case class MyPerson(name: MyId, age: Int)
    val MyPersonQuat = Quat.Product("name" -> Quat.Product("name" -> Quat.LeafProduct("first", "last"), "memberNum" -> Quat.Value), "age" -> Quat.Value)
    MyPersonQuat mustEqual quatOf[MyPerson]
    quote(query[MyPerson]).ast.quat mustEqual MyPersonQuat
  }

  "should support least upper types" - {
    val AnimalQuat = Quat.LeafProduct("name")
    val CatQuat = Quat.LeafProduct("name", "color")

    "simple reduction" in {
      AnimalQuat.leastUpperType(CatQuat).get mustEqual AnimalQuat
      CatQuat.leastUpperType(AnimalQuat).get mustEqual AnimalQuat
    }

    "in query as" in {
      trait Animal { def name: String }
      case class Cat(name: String, color: Int) extends Animal

      inline def isSpot[A <: Animal] = quote {
        (animals: Query[A]) => animals.filter(a => a.name == "Spot")
      }
      quote(isSpot[Cat](query[Cat])).ast.quat mustEqual CatQuat

      // ============= FIX Needed in Quill Core Portable ===========
      // // TODO Always returns a value since QuotationTag quat is always value which it should NOT be.
      // //      need to fix this up in regular Quill and publish new code version.
      // def isSpotRuntime[A <: Animal] = quote {
      //   (animals: Query[A]) => animals.filter(a => a.name == "Spot")
      // }
      // quote(isSpotRuntime[Cat](query[Cat])).ast.quat mustEqual CatQuat
    }
  }

  "lookup" - {
    val bar = Quat.Product("baz" -> Quat.Value)
    val foo = Quat.Product("v" -> Quat.Value, "bar" -> bar)
    val example = Quat.Product("v" -> Quat.Value, "foo" -> foo)
    "path" in {
      example.lookup("foo", true) mustEqual foo
      example.lookup(List("foo", "bar"), true) mustEqual bar
      example.lookup(List("foo", "bar", "baz"), true) mustEqual Quat.Value
      example.lookup("blah", false) mustEqual Quat.Unknown
    }
  }

  "probit" in {
    val p: Quat = Quat.Product("foo" -> Quat.Value)
    val v: Quat = Quat.Value
    p.probit mustEqual p
    val e = intercept[QuatException] {
      v.probit
    }
  }

  "rename" - {
    val prod = Quat.Product("bv" -> Quat.BooleanValue, "be" -> Quat.BooleanExpression, "v" -> Quat.Value, "p" -> Quat.Product("vv" -> Quat.Value, "pp" -> Quat.Product("ppp" -> Quat.Value)))
    val expect = Quat.Product("bva" -> Quat.BooleanValue, "be" -> Quat.BooleanExpression, "v" -> Quat.Value, "pa" -> Quat.Product("vv" -> Quat.Value, "pp" -> Quat.Product("ppp" -> Quat.Value)))
    val value = Quat.Value
    "rename field" in {
      prod.withRenames(List("bv" -> "bva", "p" -> "pa")).applyRenames mustEqual expect
    }
    val e = intercept[QuatException] {
      value.withRenames(List("foo" -> "bar"))
    }
  }

  "should serialize" - {
    // Need to import implicits from BooQuatSerializer otherwise c_jl_UnsupportedOperationException happens in JS
    import BooQuatSerializer._
    val example = Quat.Product("bv" -> Quat.BooleanValue, "be" -> Quat.BooleanExpression, "v" -> Quat.Value, "p" -> Quat.Product("vv" -> Quat.Value))
    "with boo" in {
      Quat.fromSerialized(serialize(example)) mustEqual example
    }
    // kryo tests are covered by standard JVM quill specs
  }

  "should support types in Query[T] position" - {
    "boolean value" in {
      // TODO Does the non-inline version of this work
      inline def func = quote {
        (q: Query[Boolean]) => q.filter(p => p == true)
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "boolean value - type" in {
      type Bool = Boolean
      def func = quote {
        (q: Query[Bool]) => q.filter(p => p == true)
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "value" in {
      def func = quote {
        (q: Query[Int]) => q.filter(p => p == 1)
      }
      func.ast.quat mustEqual Quat.Value
    }
    /*
    "structural with bool type" in {
    TODO Structural types are done differently in In Scala 3. Need to look into this
      import reflect.Selectable.reflectiveSelectable
      // TODO Non Inline Version also once QuotationTag issue fixed
      def func[T <: { def name: String; def isRussian: Boolean }] = quote {
        (q: Query[T]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Generic
    }
    "structural with bool indirect" in {
      TODO Structural types are done differently in In Scala 3. Need to look into this
      TODO Non Inline Version also once QuotationTag issue fixed
      type Bool = Boolean
      def func[T <: { def name: String; def isRussian: Bool }] = quote {
        (q: Query[T]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Generic
    }
     */
    "case class" in {
      case class MyPerson(name: String, isRussian: Boolean)
      def func = quote {
        (q: Query[MyPerson]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "case class with boundary" in {
      case class MyPerson(name: String, isRussian: Boolean)
      def func[T <: MyPerson] = quote {
        (q: Query[T]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Generic
    }
    "interface" in {
      trait LikePerson { def name: String; def isRussian: Boolean }
      def func = quote {
        (q: Query[LikePerson]) => q.filter(p => p.name == "Joe")
      }
      // TODO What about abstract classes? What does Flags.Abstract do?
      func.ast.quat mustEqual Quat.Generic // helloooo
    }
    "interface with boundary" in {
      trait LikePerson { def name: String; def isRussian: Boolean }
      def func[T <: LikePerson] = quote {
        (q: Query[T]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Generic
    }
    "interface with boundary boolean indirect" in {
      type Bool = Boolean
      trait LikePerson { def name: String; def isRussian: Bool }
      def func[T <: LikePerson] = quote {
        (q: Query[T]) => q.filter(p => p.name == "Joe")
      }
      func.ast.quat mustEqual Quat.Generic
    }
    "boundary with value" in {
      def func[T <: Int] = quote {
        (q: Query[T]) => q
      }
      func.ast.quat mustEqual Quat.Value
    }
    "boundary with value - boolean" in {
      def func[T <: Boolean] = quote {
        (q: Query[T]) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "boundary with value and type - boolean" in {
      type Bool = Boolean
      def func[T <: Bool] = quote {
        (q: Query[T]) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "any" in {
      // TODO Non Inline Version also once QuotationTag issue fixed
      def func = quote {
        (q: Query[Any]) => q
      }
      func.ast.quat mustEqual Quat.Generic
    }
  }

  "should support types" - {
    "boolean value" in {
      def func = quote {
        (q: Boolean) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "boolean value - type" in {
      type Bool = Boolean
      def func = quote {
        (q: Bool) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "value" in {
      def func = quote {
        (q: Int) => q
      }
      func.ast.quat mustEqual Quat.Value
    }
    "structural with bool type" in {
      // TODO Figure out issues with structural types
      // TODO Non Inline Version also once QuotationTag issue fixed
      // def func[T <: { def name: String; def isRussian: Boolean }] = quote {
      //   (q: T) => q
      // }
      // func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue).withType(Quat.Product.Type.Abstract)
    }
    "structural with bool indirect" in {
      // TODO Figure out issues with structural types
      // TODO Non Inline Version also once QuotationTag issue fixed
      // type Bool = Boolean
      // def func[T <: { def name: String; def isRussian: Bool }] = quote {
      //   (q: T) => q
      // }
      // func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "case class" in {
      case class MyPerson(name: String, isRussian: Boolean)
      def func = quote {
        (q: MyPerson) => q
      }
      func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "case class with boundary" in {
      case class MyPerson(name: String, isRussian: Boolean)
      // <LOOK INTO> This one turns out generic
      // def func[T <: MyPerson] = quote {
      //   (q: T) => q
      // }
      // func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "interface" in {
      trait LikePerson { def name: String; def isRussian: Boolean }
      def func = quote {
        (q: LikePerson) => q
      }
      func.ast.quat mustEqual Quat.Generic
    }
    "interface with boundary" in {
      // <LOOK INTO> Same thing this turn out generic
      // trait LikePerson { def name: String; def isRussian: Boolean }
      // def func[T <: LikePerson] = quote {
      //   (q: T) => q
      // }
      // func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "interface with boundary boolean indirect" in {
      type Bool = Boolean
      // <LOOK INTO> This is a generic in the new one
      // trait LikePerson { def name: String; def isRussian: Bool }
      // def func[T <: LikePerson] = quote {
      //   (q: T) => q
      // }
      // func.ast.quat mustEqual Quat.Product("name" -> Quat.Value, "isRussian" -> Quat.BooleanValue)
    }
    "boundary with value" in {
      def func[T <: Int] = quote {
        (q: T) => q
      }
      func.ast.quat mustEqual Quat.Value
    }
    "boundary with value - boolean" in {
      def func[T <: Boolean] = quote {
        (q: T) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "boundary with value and type - boolean" in {
      type Bool = Boolean
      def func[T <: Bool] = quote {
        (q: T) => q
      }
      func.ast.quat mustEqual Quat.BooleanValue
    }
    "any" in {
      def func = quote {
        (q: Any) => q
      }
      func.ast.quat mustEqual Quat.Generic
    }
  }
}
