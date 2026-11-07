package zio.quill.parser.engine

import scala.compiletime.testing.typeCheckErrors
import zio.quill.Spec

class ActionPrefilterSpec extends Spec {
  "insert returning without an argument retains its diagnostic" in {
    val errors = typeCheckErrors("""
      import zio.quill.*
      case class Person(id: Int)
      quote { query[Person].insert(_.id -> 1).returning[Int] }
    """)
    errors.map(_.message) must contain("A 'returning' clause must have arguments.")
  }

  "update returning without an argument retains its diagnostic" in {
    val errors = typeCheckErrors("""
      import zio.quill.*
      case class Person(id: Int)
      quote { query[Person].update(_.id -> 1).returning[Int] }
    """)
    errors.map(_.message) must contain("A 'returning' clause must have arguments.")
  }

  "delete returning without an argument retains its diagnostic" in {
    val errors = typeCheckErrors("""
      import zio.quill.*
      case class Person(id: Int)
      quote { query[Person].delete.returning[Int] }
    """)
    errors.map(_.message) must contain("A 'returning' clause must have arguments.")
  }
}
