package io.getquill.util

import scala.quoted.*

object ErrorMessages {

  def failMsg[DecoderT, ResultRow, Session](using Quotes, Type[DecoderT], Type[ResultRow], Type[Session]): String =
    failMsg[DecoderT, ResultRow, Session]("")

  def failMsg[DecoderT, ResultRow, Session](leafFailMsg: String)(using Quotes, Type[DecoderT], Type[ResultRow], Type[Session]): String = {
    import quotes.reflect.*
    s"""Decoder lookup failure for: ${Type.show[DecoderT]} (row-type: ${Format.TypeOf[ResultRow]}, session-type: ${Format.TypeOf[Session]}).
       |Have you imported a Decoder[${Format.TypeOf[DecoderT]}]?
       |
       |Typically field decoders are imported from the Context's companion object. For example, if you are using ZIO's Quill.Postgres,
       |import the decoders from the companion object by doing `import io.getquill.jdbczio.Quill.Postgres.*
       |
       |TODO Message about importing composite decoders
       |""".stripMargin + (
      if (leafFailMsg.nonEmpty) {
        s"""=============== Cannot Summon Decoder[${Format.TypeOf[DecoderT]}] because: ===============
           |${leafFailMsg}
           |""".stripMargin
      } else
        ""
      )
  }

}
