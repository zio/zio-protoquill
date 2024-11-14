package io.getquill.context.jdbc

import io.getquill.{H2JdbcContext, TestEntities}
import io.getquill.context.sql.{TestDecoders, TestEncoders}
import io.getquill.generic.{DecodingType, GenericDecoder}
import io.getquill.SpecEncoders
import java.sql.{Connection, PreparedStatement, ResultSet}

trait JdbcSpecEncoders extends SpecEncoders with TestEntities with TestEncoders with TestDecoders {
  type SpecSession = Connection
  type SpecPrepareRow = PreparedStatement
  type SpecResultRow = ResultSet

  given nullChecker: JdbcNullChecker = new JdbcNullChecker()
  given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific]
  given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific]
  given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific]
  given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific]
  given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific]
}

trait H2JdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.H2JdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = H2JdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = H2JdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = H2JdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = H2JdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = H2JdbcContext.booleanDecoder
}

trait PostgresJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.PostgresJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = PostgresJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = PostgresJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = PostgresJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = PostgresJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = PostgresJdbcContext.booleanDecoder
}

trait MysqlJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.MysqlJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = MysqlJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = MysqlJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = MysqlJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = MysqlJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = MysqlJdbcContext.booleanDecoder
}

trait SqliteJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.SqliteJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = SqliteJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = SqliteJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = SqliteJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = SqliteJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = SqliteJdbcContext.booleanDecoder
}

trait SqlServerJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.SqlServerJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = SqlServerJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = SqlServerJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = SqlServerJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = SqlServerJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = SqlServerJdbcContext.booleanDecoder
}

trait OracleJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.OracleJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Specific] = OracleJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Specific] = OracleJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Specific] = OracleJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Specific] = OracleJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Specific] = OracleJdbcContext.booleanDecoder
}
