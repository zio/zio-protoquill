package io.getquill.record

import scala.NamedTuple
import scala.NamedTuple.NamedTuple
import scala.compiletime._
import scala.deriving.Mirror

/**
 * Schema derivation using NamedTuple type-level operations.
 *
 * Replaces the compile-time field extraction in `ElaborateStructure.scala`
 * (the recursive `'[field *: fields]` / `'[tpe *: types]` pattern matching
 * that currently runs inside macros) with zero-macro type-level operations.
 *
 * For `case class Person(name: String, age: Int)`:
 *   - `FieldNames[Person]` = `("name", "age")`
 *   - `FieldTypes[Person]` = `(name: String, age: Int)`
 *   - `fieldNames[Person]` = `List("name", "age")` at runtime
 */
object SchemaDeriving {

  /**
   * Extract field names as a tuple of string literal types.
   */
  type FieldNames[C] = NamedTuple.Names[NamedTuple.From[C]]

  /**
   * Extract the full NamedTuple type from a case class.
   */
  type FieldTypes[C] = NamedTuple.From[C]

  /**
   * Extract field names as a runtime List[String] using compiletime operations.
   */
  inline def fieldNames[C](using m: Mirror.ProductOf[C]): List[String] = {
    fieldNamesFromTuple[m.MirroredElemLabels]
  }

  private inline def fieldNamesFromTuple[T <: Tuple]: List[String] = {
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        constValue[head].asInstanceOf[String] :: fieldNamesFromTuple[tail]
    }
  }

  /**
   * Count the number of fields in a case class at compile time.
   */
  inline def fieldCount[C](using m: Mirror.ProductOf[C]): Int = {
    constValue[Tuple.Size[m.MirroredElemTypes]]
  }
}
