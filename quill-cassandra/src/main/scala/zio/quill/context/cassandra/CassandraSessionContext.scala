package zio.quill.context.cassandra

import zio.quill.ReturnAction
import com.datastax.oss.driver.api.core.cql.{ BoundStatement, Row }
import zio.quill.NamingStrategy
import zio.quill.context.{ CassandraSession, ExecutionInfo, Context, UdtValueLookup }
import zio.quill.context.cassandra.encoding.{ CassandraTypes, Decoders, Encoders } //UdtEncoding
import zio.quill.util.ContextLogger
import zio.quill.util.Messages.fail
import zio.quill.context.RowContext

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.util.Try
import zio.quill.context.ProtoContextSecundus
import zio.quill.generic.GenericNullChecker

abstract class CassandraSessionContext[+N <: NamingStrategy]
  extends CassandraPrepareContext[N]
  with CassandraBaseContext[N]

/**
 * When using this context, we cannot encode UDTs since does not have a proper CassandraSession trait mixed in with udtValueOf.
 * Certain contexts e.g. the CassandraLagomContext does not currently have this ability.
 */
abstract class CassandraSessionlessContext[+N <: NamingStrategy]
  extends CassandraPrepareContext[N]


trait CassandraPrepareContext[+N <: NamingStrategy]
extends CassandraStandardContext[N]
with CassandraContext[N] {
  protected def prepareAsync(cql: String)(implicit executionContext: ExecutionContext): Future[BoundStatement]

  def probe(cql: String): Try[_] = {
    Try {
      Await.result(prepareAsync(cql)(ExecutionContext.Implicits.global), 1.minute)
      ()
    }
  }

  protected def prepareAsyncAndGetStatement(cql: String, prepare: Prepare, session: Session, logger: ContextLogger)(implicit executionContext: ExecutionContext): Future[BoundStatement] = {
    val prepareResult = this.prepareAsync(cql).map(row => prepare(row, session))
    val preparedRow = prepareResult.map {
      case (params, bs) =>
        logger.logQuery(cql, params)
        bs
    }
    preparedRow
  }
}

trait CassandraBaseContext[+N <: NamingStrategy] extends CassandraStandardContext[N] {
  override type Session = CassandraSession
}

trait CassandraStandardContext[+N <: NamingStrategy]
  extends CassandraRowContext
  with CassandraContext[N]
  with Context[CqlIdiom, N]
  with Encoders
  with Decoders
  with CassandraTypes {
  /*with UdtEncoding*/

  // Overriding them as defined in ProtoContextSecundus
  override type RunActionReturningResult[T] = Unit
  override type RunBatchActionReturningResult[T] = Unit

  override def executeActionReturning[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner) =
    fail("Cassandra doesn't support `returning`.")

  override def executeActionReturningMany[O](sql: String, prepare: Prepare = identityPrepare, extractor: Extractor[O], returningBehavior: ReturnAction)(info: ExecutionInfo, dc: Runner) =
    fail("Cassandra doesn't support `returningMany`.")

  override def executeBatchActionReturning[T](groups: List[BatchGroupReturning], extractor: Extractor[T])(info: ExecutionInfo, dc: Runner) =
    fail("Cassandra doesn't support `returning`.")
}

trait CassandraRowContext extends RowContext {

  val idiom = CqlIdiom

  override type PrepareRow = BoundStatement
  override type ResultRow = Row

  type BaseNullChecker = GenericNullChecker[ResultRow, Session]
  type NullChecker = CassandraNullChecker
  class CassandraNullChecker extends BaseNullChecker {
    override def apply(index: Int, row: Row): Boolean = row.isNull(index)
  }
  implicit val nullChecker: NullChecker = new CassandraNullChecker()

  // Usually this is zio.quill.context.CassandraSession so you can use udtValueOf but not always e.g. for Lagom it is different
  type Session <: UdtValueLookup
}
