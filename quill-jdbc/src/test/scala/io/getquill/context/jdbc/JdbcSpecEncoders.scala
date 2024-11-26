package io.getquill.context.jdbc

import io.getquill.{H2JdbcContext, TestEntities}
import io.getquill.context.sql.{TestDecoders, TestEncoders}
import io.getquill.generic.{DecodingType, GenericDecoder, GenericEncoder}
import io.getquill.SpecEncoders
import java.sql.{Connection, PreparedStatement, ResultSet}

trait JdbcSpecEncoders extends SpecEncoders with TestEntities with TestEncoders with TestDecoders {
  type SpecSession = Connection
  type SpecPrepareRow = PreparedStatement
  type SpecResultRow = ResultSet

  given nullChecker: JdbcNullChecker = new JdbcNullChecker()
  given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf]
  given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf]
  given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf]
  given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf]
  given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf]

  given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection]
  given longEncoder: GenericEncoder[Long, PreparedStatement, Connection]
  given intEncoder: GenericEncoder[Int, PreparedStatement, Connection]
  given stringEncoder: GenericEncoder[String, PreparedStatement, Connection]
  given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection]
}

trait H2JdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.H2JdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = H2JdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = H2JdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = H2JdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = H2JdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = H2JdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = H2JdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = H2JdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = H2JdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = H2JdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = H2JdbcContext.booleanEncoder
}

trait PostgresJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.PostgresJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = PostgresJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = PostgresJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = PostgresJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = PostgresJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = PostgresJdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = PostgresJdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = PostgresJdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = PostgresJdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = PostgresJdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = PostgresJdbcContext.booleanEncoder
}

trait MysqlJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.MysqlJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = MysqlJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = MysqlJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = MysqlJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = MysqlJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = MysqlJdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = MysqlJdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = MysqlJdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = MysqlJdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = MysqlJdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = MysqlJdbcContext.booleanEncoder
}

trait SqliteJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.SqliteJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = SqliteJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = SqliteJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = SqliteJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = SqliteJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = SqliteJdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = SqliteJdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = SqliteJdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = SqliteJdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = SqliteJdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = SqliteJdbcContext.booleanEncoder
}

trait SqlServerJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.SqlServerJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = SqlServerJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = SqlServerJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = SqlServerJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = SqlServerJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = SqlServerJdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = SqlServerJdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = SqlServerJdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = SqlServerJdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = SqlServerJdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = SqlServerJdbcContext.booleanEncoder
}

trait OracleJdbcSpecEncoders extends JdbcSpecEncoders {
  import io.getquill.OracleJdbcContext
  override given optionDecoder[T](using d: GenericDecoder[ResultSet, Connection, T, ?]): GenericDecoder[ResultSet, Connection, Option[T], DecodingType.Leaf] = OracleJdbcContext.optionDecoder(d)
  override given longDecoder: GenericDecoder[ResultSet, Connection, Long, DecodingType.Leaf] = OracleJdbcContext.longDecoder
  override given intDecoder: GenericDecoder[ResultSet, Connection, Int, DecodingType.Leaf] = OracleJdbcContext.intDecoder
  override given stringDecoder: GenericDecoder[ResultSet, Connection, String, DecodingType.Leaf] = OracleJdbcContext.stringDecoder
  override given booleanDecoder: GenericDecoder[ResultSet, Connection, Boolean, DecodingType.Leaf] = OracleJdbcContext.booleanDecoder

  override given optionEncoder[T](using e: GenericEncoder[T, PreparedStatement, Connection]): GenericEncoder[Option[T], PreparedStatement, Connection] = OracleJdbcContext.optionEncoder(e)
  override given longEncoder: GenericEncoder[Long, PreparedStatement, Connection] = OracleJdbcContext.longEncoder
  override given intEncoder: GenericEncoder[Int, PreparedStatement, Connection] = OracleJdbcContext.intEncoder
  override given stringEncoder: GenericEncoder[String, PreparedStatement, Connection] = OracleJdbcContext.stringEncoder
  override given booleanEncoder: GenericEncoder[Boolean, PreparedStatement, Connection] = OracleJdbcContext.booleanEncoder
}
