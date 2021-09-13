package io.getquill.ported.quotationspec

import io.getquill.parser.SerializeAst

/** In quotation tests, do not serialize the AST since
  * we want to test the lifting/unlifting mechanism.
  */
trait NonSerializingQuotation:
  inline given SerializeAst = SerializeAst.None