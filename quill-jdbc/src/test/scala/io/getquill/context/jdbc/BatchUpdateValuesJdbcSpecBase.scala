package io.getquill.context.jdbc

import io.getquill.context.jdbc.JdbcContext
import io.getquill.context.sql.base.BatchUpdateValuesSpec

trait BatchUpdateValuesJdbcSpecBase extends BatchUpdateValuesSpec with JdbcSpecEncoders {
  //case class Contact(firstName: String, lastName: String, age: Int)
  //case class Name(first: String, last: String)
  //case class ContactTable(name: Option[Name], age: Int)
  //case class FirstName(firstName: Option[String]) extends Embedded
  //case class LastName(lastName: Option[String]) extends Embedded
  //case class NameEmb(first: FirstName, last: LastName) extends Embedded
  //case class DeepContact(name: Option[NameEmb], age: Int)

  given JdbcContext.GenericDecoder[Contact] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[Name] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[ContactTable] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[FirstName] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[LastName] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[NameEmb] = JdbcContext.deriveDecoder
  given JdbcContext.GenericDecoder[DeepContact] = JdbcContext.deriveDecoder
}
