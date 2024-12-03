package io.getquill.context.jdbc

import io.getquill.Spec

trait JdbcVerbs extends Spec {
  type Session = java.sql.Connection
  type ResultRow = java.sql.ResultSet
  type PrepareRow = java.sql.PreparedStatement
}
