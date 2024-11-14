package io.getquill.context.sql

import io.getquill._
import io.getquill.norm.EqualityBehavior.NonAnsiEquality
import io.getquill.norm.EqualityBehavior

trait NonAnsiMirrorSqlDialect extends MirrorSqlDialect {
  override def equalityBehavior: EqualityBehavior = NonAnsiEquality
}
object NonAnsiMirrorSqlDialect extends NonAnsiMirrorSqlDialect {
  override def prepareForProbing(string: String) = string
}

class NonAnsiTestContextTemplate[+Naming <: NamingStrategy](naming: Naming)
  extends SqlMirrorContext(NonAnsiMirrorSqlDialect, naming) {
}

object nonAnsiTestContext extends NonAnsiTestContextTemplate(Literal)