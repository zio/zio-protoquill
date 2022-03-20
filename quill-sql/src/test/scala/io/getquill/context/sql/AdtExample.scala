package io.getquill.context.sql

import java.time.LocalDate
import io.getquill._ //

object StaticDateExample:
  case class Person(name: String, birthDate: LocalDate)

  val ctx = new SqlMirrorContext(PostgresDialect, Literal)
  import ctx._
  inline def staticDate = infix"'19820101'".as[LocalDate]
  // Makes Sense: inline def staticDate = LocalDate(1982,01,01)
  // Makes NO Sense: inline def staticDate = "'19820101'".asInstanceOf[String]

  val result = run(query[Person].filter(p => p.birthDate == staticDate))
