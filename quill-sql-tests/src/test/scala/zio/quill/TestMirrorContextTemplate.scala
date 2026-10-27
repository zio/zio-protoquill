package zio.quill

import zio.quill.context.{ CanReturnField, CanReturnMultiField, CannotReturn }
import zio.quill.idiom.Idiom
import zio.quill.norm.EqualityBehavior
import zio.quill.norm.EqualityBehavior.NonAnsiEquality
import zio.quill.context.sql.TestEncoders
import zio.quill.context.sql.TestDecoders

class TestMirrorContextTemplate[+Dialect <: Idiom, +Naming <: NamingStrategy](dialect: Dialect, naming: Naming)
  extends MirrorContext[Dialect, Naming](dialect, naming) with TestEntities {

  def withDialect[I <: Idiom](dialect: I)(f: TestMirrorContextTemplate[I, Naming] => Any): Unit = {
    val ctx = new TestMirrorContextTemplate[I, Naming](dialect, naming)
    f(ctx)
    ctx.close
  }
}

// Mirror idiom supporting only single-field returning clauses
trait MirrorIdiomReturningSingle extends MirrorIdiomBase with CanReturnField
object MirrorIdiomReturningSingle extends MirrorIdiomReturningSingle

// Mirror idiom supporting only multi-field returning clauses
trait MirrorIdiomReturningMulti extends MirrorIdiomBase with CanReturnMultiField
object MirrorIdiomReturningMulti extends MirrorIdiomReturningMulti

// Mirror idiom not supporting any returns
trait MirrorIdiomReturningUnsupported extends MirrorIdiomBase with CannotReturn
object MirrorIdiomReturningUnsupported extends MirrorIdiomReturningUnsupported
