package io.getquill.doobie

import io.getquill._
import io.getquill.context.jdbc._

object DoobieContext {

  class H2[+N <: NamingStrategy](val naming: N) extends DoobieContextBase[H2Dialect, N] with H2JdbcTypes[H2Dialect, N]:
    val idiom: H2Dialect = H2Dialect

  class MySQL[+N <: NamingStrategy](val naming: N)
      extends DoobieContextBase[MySQLDialect, N]
      with MysqlJdbcTypes[MySQLDialect, N]:
    val idiom: MySQLDialect = MySQLDialect

  class Oracle[+N <: NamingStrategy](val naming: N)
      extends DoobieContextBase[OracleDialect, N]
      with OracleJdbcTypes[OracleDialect, N]:
    val idiom: OracleDialect = OracleDialect

  class Postgres[+N <: NamingStrategy](val naming: N)
      extends DoobieContextBase[PostgresDialect, N]
      with PostgresJdbcTypes[PostgresDialect, N]:
    val idiom: PostgresDialect = PostgresDialect

  class SQLite[+N <: NamingStrategy](val naming: N)
      extends DoobieContextBase[SqliteDialect, N]
      with SqliteJdbcTypes[SqliteDialect, N]:
    val idiom: SqliteDialect = SqliteDialect

  class SQLServer[+N <: NamingStrategy](val naming: N)
      extends DoobieContextBase[SQLServerDialect, N]
      with SqlServerJdbcTypes[SQLServerDialect, N]:
    val idiom: SQLServerDialect = SQLServerDialect

}
