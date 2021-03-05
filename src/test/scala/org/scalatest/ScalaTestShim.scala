package org.scalatest

trait BeforeAndAfterAll {
}
object freespec {
  trait AnyFreeSpec extends ShimExtensions {
  }
}
object matchers {
  object must {
    trait Matchers
  }
}

enum AssertThrowsStates:
  case Success
  case NoException
  case WrongException(e: Throwable)

object ScalaTestShim {
  // todo go through classes by name in the classpath, see which ones end with
  // "Test". Instantiate them. The 'in' result call should result with an object
  // like class Test { def test: Any } being instantiated for every test and then
  // added to a list in which they are all then executed inside of.
}

trait ShimExtensions {
  extension (str: String) {
    infix def -(any: =>Any) = println(any)
    infix def in (any: =>Any) = println(any)
  }
  extension [T] (some: Any) {
    def mustEqual(other: Any) =
      if (some != other) throw new IllegalArgumentException(s"${some} did not match ${other}")

    def assertThrows(any: Any)(using tag: scala.reflect.ClassTag[T]) =
      var state = AssertThrowsStates.NoException
      try { any } catch { 
        case e if (tag.runtimeClass.isAssignableFrom(e.getClass)) =>
          state = AssertThrowsStates.Success
        case other =>
          state = AssertThrowsStates.WrongException(other)
      }
      state match
        case AssertThrowsStates.NoException => throw new RuntimeException("No exception thrown")
        case AssertThrowsStates.WrongException(e) =>  throw new RuntimeException(s"Wrong exception thrown: ${e}, expected ${tag.runtimeClass}")
        case _ => // Otherwise success
      

    def must(must: Must): Unit = must.execute(some)
  }

  def inside(any: Any)(func: PartialFunction[Any, Any]) =
    func.lift(any)

  //def matchPattern(any: Any) = new Must { def value = "foo" }
  def matchPattern(func: PartialFunction[Any, Any]) =
    new Must {
      def execute(any: Any) =
        func.lift(any) match {
          case Some(value) => println("Succeeded!")
          case None => println(s"The value did not match the pattern")
        }
    }
}

trait Must {
  def execute(any: Any): Unit
}

trait Inside {
  
}
