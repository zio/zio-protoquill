package io.getquill

import io.getquill.idiom.{Idiom => BaseIdiom}
import io.getquill.context.sql.SqlContext
import io.getquill.context.mirror.ArrayMirrorEncoding
import io.getquill.context.AstSplicing
import io.getquill.context.mirror.MirrorSession

/** Workaround for IntelliJ SCL-20185. Inheriting MirrorContextBase directly so that `run` methods have autocomplete. */
class SqlMirrorContext[Idiom <: BaseIdiom, Naming <: NamingStrategy](val idiom: Idiom, val naming: Naming)
    extends MirrorContextBase[Idiom, Naming]
    with AstSplicing
    with SqlContext[Idiom, Naming]
    with ArrayMirrorEncoding:

  val session: MirrorSession = MirrorSession("DefaultMirrorContextSession")
end SqlMirrorContext
