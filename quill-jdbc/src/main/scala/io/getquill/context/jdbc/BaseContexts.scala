package io.getquill.context.jdbc

import io.getquill._

trait PostgresJdbcContextBase[+D <: PostgresDialect, +N <: NamingStrategy]
  extends JdbcContextBase[D, N]

trait H2JdbcContextBase[+D <: H2Dialect, +N <: NamingStrategy]
  extends JdbcContextBase[D, N]

trait MysqlJdbcContextBase[+D <: MySQLDialect, +N <: NamingStrategy]
  extends JdbcContextBase[D, N]

trait SqliteJdbcContextBase[+D <: SqliteDialect, +N <: NamingStrategy]
  extends SqliteExecuteOverride[D, N]
  with JdbcContextBase[D, N]

trait SqlServerJdbcContextBase[+D <: SQLServerDialect, +N <: NamingStrategy]
  extends SqlServerExecuteOverride[N]
  with JdbcContextBase[D, N]

trait OracleJdbcContextBase[+D <: OracleDialect, +N <: NamingStrategy]
  extends JdbcContextBase[D, N]
