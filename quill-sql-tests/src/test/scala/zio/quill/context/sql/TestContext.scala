package zio.quill.context.sql

import zio.quill._
import zio.quill.norm.EqualityBehavior.NonAnsiEquality
import zio.quill.norm.EqualityBehavior

trait NonAnsiMirrorSqlDialect extends MirrorSqlDialect {
  override def equalityBehavior: EqualityBehavior = NonAnsiEquality
}
object NonAnsiMirrorSqlDialect extends NonAnsiMirrorSqlDialect {
  override def prepareForProbing(string: String) = string
}

class NonAnsiTestContextTemplate[+Naming <: NamingStrategy](naming: Naming)
  extends SqlMirrorContext(NonAnsiMirrorSqlDialect, naming)
  with TestEntities
  with TestEncoders
  with TestDecoders {
}

object nonAnsiTestContext extends NonAnsiTestContextTemplate(Literal)