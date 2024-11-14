package io.getquill.context.jdbc

import io.getquill.context.sql.{ TestDecoders, TestEncoders }
import io.getquill.{ Literal, SqliteJdbcContext, TestEntities }

package object sqlite {

  object testContext extends SqliteJdbcContext(Literal, "testSqliteDB")

}
