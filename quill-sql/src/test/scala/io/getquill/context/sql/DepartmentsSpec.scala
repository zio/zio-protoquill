package io.getquill.context.sql

import io.getquill.Spec
import io.getquill.Query
import io.getquill.context.Context
import io.getquill._

trait DepartmentsSpec extends Spec {

  val context: Context[_, _]

  import context._

  case class Department(dpt: String)
  case class Employee(emp: String, dpt: String)
  case class Task(emp: String, tsk: String)

  inline def departmentInsert =
    quote {
      (dpt: Department) => query[Department].insertValue(dpt)
    }

  val departmentEntries =
    List(
      Department("Product"),
      Department("Quality"),
      Department("Research"),
      Department("Sales")
    )

  inline def employeeInsert =
    quote {
      (emp: Employee) => query[Employee].insertValue(emp)
    }

  val employeeEntries =
    List(
      Employee("Alex", "Product"),
      Employee("Bert", "Product"),
      Employee("Cora", "Research"),
      Employee("Drew", "Research"),
      Employee("Edna", "Research"),
      Employee("Fred", "Sales")
    )

  inline def taskInsert =
    quote {
      (tsk: Task) => query[Task].insertValue(tsk)
    }

  val taskEntries =
    List(
      Task("Alex", "build"),
      Task("Bert", "build"),
      Task("Cora", "abstract"),
      Task("Cora", "build"),
      Task("Cora", "design"),
      Task("Drew", "abstract"),
      Task("Drew", "design"),
      Task("Edna", "abstract"),
      Task("Edna", "call"),
      Task("Edna", "design"),
      Task("Fred", "call")
    )

  inline def `Example 8 expertise naive` =
    quote {
      (u: String) =>
        for {
          d <- query[Department] if (
            (for {
              e <- query[Employee] if (
                e.dpt == d.dpt && (
                  for {
                    t <- query[Task] if (e.emp == t.emp && t.tsk == u)
                  } yield {}
                ).isEmpty
              )
            } yield {}).isEmpty
          )
        } yield d.dpt
    }

  val `Example 8 param` = "abstract"

  val `Example 8 expected result` = List("Quality", "Research")

  inline def any[T] =
    quote { (xs: Query[T]) => (p: T => Boolean) =>
      (for {
        x <- xs if (p(x))
      } yield {}).nonEmpty
    }

  inline def nestedOrg =
    quote {
      for {
          d <- query[Department]
      } yield {
          (d.dpt,
          for {
              e <- query[Employee] if (d.dpt == e.dpt)
          } yield {
              (e.emp,
              for {
                  t <- query[Task] if (e.emp == t.emp)
              } yield {
                  t.tsk
              })
          })
      }
    }

  // TODO Typing error if add `quote` around this. Examine that more closely
  inline def all[T] =
    (xs: Query[T]) => (p: T => Boolean) =>
      !any(xs)(x => !p(x))


  inline def contains[T] =
    quote { (xs: Query[T]) => (u: T) =>
      any(xs)(x => x == u)
    }

  inline def `Example 9 expertise` = {
    quote {
      (u: String) =>
        for {
          (dpt, employees) <- nestedOrg if (all(employees) { case (emp, tasks) => contains(tasks)(u) })
        } yield {
          dpt
        }
    }
  }

  val `Example 9 param` = "abstract"

  val `Example 9 expected result` = List("Quality", "Research")
}
