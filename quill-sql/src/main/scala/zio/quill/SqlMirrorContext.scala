package zio.quill

import zio.quill.idiom.{Idiom => BaseIdiom}
import zio.quill.context.sql.SqlContext
import zio.quill.context.mirror.ArrayMirrorEncoding
import zio.quill.context.AstSplicing
import zio.quill.context.mirror.MirrorSession

/** Workaround for IntelliJ SCL-20185. Inheriting MirrorContextBase directly so that `run` methods have autocomplete. */
class SqlMirrorContext[+Idiom <: BaseIdiom, +Naming <: NamingStrategy](val idiom: Idiom, val naming: Naming)
    extends MirrorContextBase[Idiom, Naming]
    with AstSplicing
    with SqlContext[Idiom, Naming]
    with ArrayMirrorEncoding {

  val session: MirrorSession = MirrorSession("DefaultMirrorContextSession")
} // end SqlMirrorContext
