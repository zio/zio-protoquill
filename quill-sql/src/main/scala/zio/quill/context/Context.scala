package zio.quill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import zio.quill.ReturnAction
import zio.quill.generic.EncodingDsl
import zio.quill.Quoted
import zio.quill.QueryMeta
import zio.quill.generic._
import zio.quill.context.mirror.MirrorDecoders
import zio.quill.context.mirror.Row
import zio.quill.generic.GenericDecoder
import zio.quill.generic.DecodingType
import zio.quill.Planter
import zio.quill.ast.Ast
import zio.quill.ast.ScalarTag
import scala.quoted._
import zio.quill.idiom.Idiom
import zio.quill.ast.{Transform, QuotationTag}
import zio.quill.QuotationLot
import zio.quill.metaprog.QuotedExpr
import zio.quill.metaprog.PlanterExpr
import zio.quill.idiom.ReifyStatement
import zio.quill.Query
import zio.quill.metaprog.etc.MapFlicer
import zio.quill.util.Messages.fail
import java.io.Closeable
import zio.quill.util.Format
import zio.quill.QAC
import zio.quill.Action
import zio.quill.ActionReturning
import zio.quill.BatchAction
import zio.quill.Literal
import scala.annotation.targetName
import zio.quill.NamingStrategy
import zio.quill.idiom.Idiom
import zio.quill.context.ProtoContextSecundus
import zio.quill.context.AstSplicing
import zio.quill.context.RowContext
import zio.quill.metaprog.etc.ColumnsFlicer
import zio.quill.context.Execution.ElaborationBehavior
import zio.quill.OuterSelectWrap
import zio.quill.generic.DecodeAlternate
import com.typesafe.scalalogging.Logger
import zio.quill.DynamicInsert
import zio.quill.DynamicEntityQuery
import zio.quill.DynamicUpdate
import zio.quill.Insert
import zio.quill.Update
import zio.quill.EntityQuery

trait ContextStandard[+Idiom <: zio.quill.idiom.Idiom, +Naming <: NamingStrategy]
    extends Context[Idiom, Naming]
    with ContextVerbPrepareLambda[Idiom, Naming]

trait Context[+Dialect <: Idiom, +Naming <: NamingStrategy]
    extends ProtoContextSecundus[Dialect, Naming] with EncodingDsl with Closeable {
  self =>

  /**
   * Base type used to determine whether there is an execution context that needs to be summoned to perform
   * execution methods e.g. in the PostgresJasync contexts that use Scala Futures that need an ExecutionContext.
   */
  type RunnerBehavior <: RunnerSummoningBehavior
  protected def context: Runner = fail(s"Runner method not implemented for '${this.getClass.getName}' Context")

  implicit inline def dec[T]: GenericDecoder[ResultRow, Session, T, DecodingType.Generic] = ${ GenericDecoder.summon[T, ResultRow, Session] }

  // def probe(statement: String): Try[_]
  // todo add 'prepare' i.e. encoders here
  // def executeAction(cql: String, prepare: Prepare = identityPrepare)(implicit executionContext: ExecutionContext): Result[RunActionResult]

  inline def lift[T](inline runtimeValue: T): T =
    ${ LiftMacro[T, PrepareRow, Session]('runtimeValue) } // Needs PrepareRow in order to be able to summon encoders

  inline def liftQuery[U[_] <: Iterable[_], T](inline runtimeValue: U[T]): Query[T] =
    ${ LiftQueryMacro[T, U, PrepareRow, Session]('runtimeValue) }

  // Originally insertValue/updateValue for EntityQuery[Query[T]]/Quoted[EntityQuery[Query[T]]] lived in
  // Dsl.scala and did not require a context but when insertValue/updateValue for DynamicEntityQuery was introduced
  // it caused oddities with how Scala 3 resolves/overrides extension methods. This required
  // all of them to be moved to Context.scala or else compilation would fail because certain
  // insertValue/updateValue methods were not found.
  extension [T](inline dynamicQuery: DynamicEntityQuery[T]) {
    inline def insertValue(value: T): DynamicInsert[T] =
      DynamicInsert(zio.quill.quote(insertValueDynamic(dynamicQuery.q)(lift(value))))
    inline def updateValue(value: T): DynamicUpdate[T] =
      DynamicUpdate(zio.quill.quote(updateValueDynamic(dynamicQuery.q)(lift(value))))
  }

  extension [T](inline entity: EntityQuery[T]) {
    inline def insertValue(inline value: T): Insert[T] = ${ InsertUpdateMacro.static[T, Insert]('entity, 'value) }
    inline def updateValue(inline value: T): Update[T] = ${ InsertUpdateMacro.static[T, Update]('entity, 'value) }
    private[quill] inline def insertValueDynamic(inline value: T): Insert[T] = ${ InsertUpdateMacro.dynamic[T, Insert]('entity, 'value) }
    private[quill] inline def updateValueDynamic(inline value: T): Update[T] = ${ InsertUpdateMacro.dynamic[T, Update]('entity, 'value) }
  }

  extension [T](inline quotedEntity: Quoted[EntityQuery[T]]) {
    inline def insertValue(inline value: T): Insert[T] = zio.quill.unquote[EntityQuery[T]](quotedEntity).insertValue(value)
    inline def updateValue(inline value: T): Update[T] = zio.quill.unquote[EntityQuery[T]](quotedEntity).updateValue(value)
    private[quill] inline def insertValueDynamic(inline value: T): Insert[T] = zio.quill.unquote[EntityQuery[T]](quotedEntity).insertValueDynamic(value)
    private[quill] inline def updateValueDynamic(inline value: T): Update[T] = zio.quill.unquote[EntityQuery[T]](quotedEntity).updateValueDynamic(value)
  }

  extension [T](inline q: Query[T]) {

    /**
     * When using this with FilterColumns make sure it comes FIRST. Otherwise the columns are you filtering
     * may have been nullified in the SQL before the filteration has actually happened.
     */
    inline def filterByKeys(inline map: Map[String, Any]) =
      q.filter(p => MapFlicer[T, PrepareRow, Session](p, map))

    inline def filterColumns(inline columns: List[String]) =
      q.map(p => ColumnsFlicer[T, PrepareRow, Session](p, columns))
  }

  /*
   * In previous version of ProtoQuill there was no InternalApi and the `run` methods were used
   * on a context directly. This caused a problem where doing something like:
   *   val list: List[RightType] = run(query[WrongType])
   * Will yield:
   *   Found: ctx.Result[List[WrongType]] Required: List[RightType]
   *
   * This was even worse if the types were more complex e.g. for ZIO:
   *   val list: ZIO[Has[DataSource], SQLException, List[RightType]] = run(query[WrongType])
   *
   * Will yield:
   *   Found: QuillContext.Result[List[WrongType]] Required: ZIO[Has[DataSource], SQLException, List[RightType]]
   *
   * This is quite confusing. Therefore we define the methods in an object and then
   * delegate to these in the individual contexts.
   */
  object InternalApi {
    /** Internal API that cannot be made private due to how inline functions */
    inline def _summonRunner() = DatasourceContextInjectionMacro[RunnerBehavior, Runner, self.type](context)

    inline def runQueryDefault[T](inline quoted: Quoted[Query[T]]): Result[RunQueryResult[T]] =
      runQuery(quoted, OuterSelectWrap.Default)

    // Must be lazy since idiom/naming are null (in some contexts) initially due to initialization order
    private lazy val make = ContextOperation.Factory[Dialect, Naming, PrepareRow, ResultRow, Session, self.type](self.idiom, self.naming)

    inline def runQuery[T](inline quoted: Quoted[Query[T]], inline wrap: OuterSelectWrap): Result[RunQueryResult[T]] = {
      val ca = make.op[Nothing, T, Result[RunQueryResult[T]]] { arg =>
        val simpleExt = arg.extractor.requireSimple()
        self.executeQuery(arg.sql, arg.prepare, simpleExt.extract)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(ca)(quoted, None, wrap)
    }

    inline def runQuerySingle[T](inline quoted: Quoted[T]): Result[RunQuerySingleResult[T]] = {
      val ca = make.op[Nothing, T, Result[RunQuerySingleResult[T]]] { arg =>
        val simpleExt = arg.extractor.requireSimple()
        self.executeQuerySingle(arg.sql, arg.prepare, simpleExt.extract)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(ca)(QuerySingleAsQuery(quoted), None)
    }

    inline def runAction[E](inline quoted: Quoted[Action[E]]): Result[RunActionResult] = {
      val ca = make.op[E, Any, Result[RunActionResult]] { arg =>
        self.executeAction(arg.sql, arg.prepare)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(ca)(quoted, None)
    }

    inline def runActionReturning[E, T](inline quoted: Quoted[ActionReturning[E, T]]): Result[RunActionReturningResult[T]] = {
      val ca = make.op[E, T, Result[RunActionReturningResult[T]]] { arg =>
        // Need an extractor with special information that helps with the SQL returning specifics
        val returningExt = arg.extractor.requireReturning()
        self.executeActionReturning(arg.sql, arg.prepare, returningExt.extract, returningExt.returningBehavior)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(ca)(quoted, None)
    }

    inline def runActionReturningMany[E, T](inline quoted: Quoted[ActionReturning[E, List[T]]]): Result[RunActionReturningResult[List[T]]] = {
      val ca = make.op[E, T, Result[RunActionReturningResult[List[T]]]] { arg =>
        // Need an extractor with special information that helps with the SQL returning specifics
        val returningExt = arg.extractor.requireReturning()
        self.executeActionReturningMany(arg.sql, arg.prepare, returningExt.extract, returningExt.returningBehavior)(arg.executionInfo, _summonRunner())
      }
      QueryExecution.apply(ca)(quoted, None)
    }

    inline def runBatchAction[I, A <: Action[I] & QAC[I, Nothing]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): Result[RunBatchActionResult] = {
      val ca = make.batch[I, Nothing, A, Result[RunBatchActionResult]] { arg =>
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val groups = arg.groups.map((sql, prepare) => BatchGroup(sql, prepare, Nil))
        self.executeBatchAction(groups.toList)(arg.executionInfo, _summonRunner())
      }
      QueryExecutionBatch.apply(ca, rowsPerBatch)(quoted)
    }

    inline def runBatchActionReturning[I, T, A <: Action[I] & QAC[I, T]](inline quoted: Quoted[BatchAction[A]], rowsPerBatch: Int): Result[RunBatchActionReturningResult[T]] = {
      val ca = make.batch[I, T, A, Result[RunBatchActionReturningResult[T]]] { arg =>
        val returningExt = arg.extractor.requireReturning()
        // Supporting only one top-level query batch group. Don't know if there are use-cases for multiple queries.
        val groups = arg.groups.map((sql, prepare) => BatchGroupReturning(sql, returningExt.returningBehavior, prepare, Nil))
        self.executeBatchActionReturning[T](groups.toList, returningExt.extract)(arg.executionInfo, _summonRunner())
      }
      QueryExecutionBatch.apply(ca, rowsPerBatch)(quoted)
    }
  } // end InternalApi

  protected def handleSingleResult[T](sql: String, list: List[T]) =
    list match {
      case Nil =>
        fail(s"Expected a single result from the query: `${sql}` but got a empty result-set!")
      case value :: Nil => value
      case other        =>
        // Note. If we want to cross-compile to ScalaJS this will only work for the JVM variant. Have a look at ContextLog
        // in Scala2-Quill for an approach on how to do that.
        Logger(s"Expected a single result from the query: `${sql}` but got: ${abbrevList(other)}. Only the 1st result will be returned!")
        other.head
    }

  private def abbrevList[T](list: List[T]) =
    if (list.length > 10)
      list.take(10).mkString("List(", ",", "...)")
    else
      list.toString()

  // Can close context. Does nothing by default.
  def close(): Unit = ()
} // end Context
