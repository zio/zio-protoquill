package org.scalatest

trait Assertion
trait BeforeAndAfterEach
trait BeforeAndAfter
trait BeforeAndAfterAll
trait Inside


trait Must {
  def execute(any: Any): Unit
}

trait Should {
  def execute(any: Any): Unit
}




object freespec {
  trait AnyFreeSpec extends ShimExtensions {
    def convertToEqualizer[T](left: T): Equalizer[T] = ???
  }
}
object matchers {
  object must {
    trait Matchers
    object Matchers extends Matchers
  }
  object should {
    trait Matchers
    object Matchers extends Matchers
  }
}

trait MustContainResult {
  def theSameElementsAs(right: Any): Unit = ???
  def theSameElementsInOrderAs(right: Any): Unit = ???
}

trait ShouldContainResult {
  def theSameElementsAs(right: Any): Unit = ???
  def theSameElementsInOrderAs(right: Any): Unit = ???
}

trait MustNotResult {
  def be(right: Any): Unit = ???
}

trait ShouldNotResult {
  def be(right: Any): Unit = ???
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

  class Equalizer[T](value: T)

  def a[T] = ???
  def an[T] = ???
  def empty: Any = ???

  sealed trait NotWord
  object not extends NotWord

  sealed trait ContainWord
  object contain extends ContainWord

  sealed trait MustBeWord
  object defined extends MustBeWord

  sealed trait MustNotWord
  object compile extends MustNotWord

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


    def mustNot(value: MustNotWord) = ???
    def mustBe(other: MustBeWord) = ???
    def mustBe(other: Any) = ???

    def must(must: ContainWord): MustContainResult = ???
    def must(must: NotWord): MustNotResult = ???
    def must(must: Must): Unit = must.execute(some)

    def should(must: ContainWord): ShouldContainResult = ???
    def should(must: NotWord): ShouldNotResult = ???
    def should(must: Should): Unit = must.execute(some)
  }

  def inside(any: Any)(func: PartialFunction[Any, Any]) =
    func.lift(any)

  def intercept[T](any: Any) = println(any)

  def fail(str: String): Nothing = throw new RuntimeException(s"Failed with: ${str}")

  protected def beforeEach(): Unit = ()
  protected def beforeAll(): Unit = ()
  def before(f: Any): Unit = ()

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
