package io.getquill.record

import scala.NamedTuple
import scala.NamedTuple.NamedTuple

/** Query-phase type wrapper: erased to FieldExpr at runtime. */
type Col[A] = FieldExpr

/**
 * A `Selectable`-based record that uses NamedTuple.From[C] to derive field access
 * at the type level rather than through macro AST inspection.
 *
 * The type parameter `W[_]` wraps each field type so the compiler sees the correct
 * runtime representation. For query building, use `Record[C, Col]` which maps all
 * field types to `FieldExpr`, matching what `selectDynamic` actually returns.
 *
 * For `case class Person(name: String, age: Int)` with `W = Col`:
 *   - `Fields` becomes `(name: Col[String], age: Col[Int])` = `(name: FieldExpr, age: FieldExpr)`
 *   - `record.name` returns `FieldExpr("p", "name")` at runtime
 *   - The compiler checks `name: FieldExpr` at the type level — no ClassCastException
 */
class Record[C, W[_]](val entityName: String, val alias: String) extends Selectable {
  type Fields = NamedTuple.Map[NamedTuple.From[C], W]

  def selectDynamic(name: String): Any = {
    FieldExpr(alias, name)
  }
}
