package io.getquill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import java.io.Closeable
import scala.compiletime.summonFrom
import scala.util.Try
import io.getquill.{ReturnAction}
import io.getquill.generic.EncodingDsl
import io.getquill.Quoted
import io.getquill.QueryMeta
import io.getquill.generic._
import io.getquill.context.mirror.MirrorDecoders
import io.getquill.context.mirror.Row
import io.getquill.generic.GenericDecoder
import io.getquill.generic.GenericEncoder
import io.getquill.Planter
import io.getquill.EagerPlanter
import io.getquill.InjectableEagerPlanter
import io.getquill.LazyPlanter
import io.getquill.ast.Ast
import io.getquill.ast.Filter
import io.getquill.ast.Entity
import io.getquill.ast.ScalarTag
import io.getquill.ast.Returning
import io.getquill.ast.ReturningGenerated
import io.getquill.ast
import scala.quoted._
import io.getquill.ast.{Transform, QuotationTag}
import io.getquill.QuotationLot
import io.getquill.metaprog.QuotedExpr
import io.getquill.metaprog.PlanterExpr
import io.getquill.metaprog.EagerEntitiesPlanterExpr
import io.getquill.Planter
import io.getquill.idiom.ReifyStatement
import io.getquill.Query
import io.getquill.Action
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.metaprog.Extractors._
import io.getquill.BatchAction
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.util.Format
import io.getquill.context.LiftMacro
import io.getquill.parser.Unlifter
import io.getquill._
import io.getquill.QAC
import io.getquill.parser.Lifter
import io.getquill.metaprog.InjectableEagerPlanterExpr
import _root_.io.getquill.norm.BetaReduction
import io.getquill.context.Execution.ElaborationBehavior
import io.getquill.quat.Quat
import io.getquill.quat.QuatMaking
import io.getquill.metaprog.EagerListPlanterExpr
import io.getquill.metaprog.EagerPlanterExpr
import io.getquill.metaprog.SummonTranspileConfig
import io.getquill.norm.TranspileConfig
import io.getquill.metaprog.TranspileConfigLiftable
import io.getquill.idiom.Token
import io.getquill.context.QueryExecutionBatchModel.SingleEntityLifts
import io.getquill.context.QueryExecutionBatchModel.BatchingBehavior
import io.getquill.context.QueryExecutionBatchModel.BatchExtractBehavior
import scala.util.Right
import scala.util.Left
import com.typesafe.scalalogging.Logger
import io.getquill.util.ContextLogger
import io.getquill.context.Execution.ExtractBehavior
import io.getquill.util.TraceConfig

object QueryExecutionBatchIteration {

  private[getquill] val logger = ContextLogger(classOf[QueryExecutionBatchIteration.type])

  def apply[PrepareRow, Session](
      idiom: io.getquill.idiom.Idiom,
      query: Unparticular.Query,
      perRowLifts: List[SingleEntityLifts],
      otherLifts: List[Planter[?, ?, ?]],
      originalEntityLifts: List[InjectableEagerPlanter[_, _, _]],
      liftingPlaceholder: Int => String,
      emptyContainsToken: Token => Token,
      batchingBehavior: BatchingBehavior,
      extractBehavior: BatchExtractBehavior
  )(traceConfig: TraceConfig): List[(String, List[(PrepareRow, Session) => (List[Any], PrepareRow)])] =
    new Executor(
      idiom,
      query,
      perRowLifts,
      otherLifts,
      originalEntityLifts,
      liftingPlaceholder,
      emptyContainsToken,
      batchingBehavior,
      extractBehavior,
      traceConfig
    ).apply()

  private[getquill] class Executor[PrepareRow, Session](
      idiom: io.getquill.idiom.Idiom,
      query: Unparticular.Query,
      perRowLifts: List[SingleEntityLifts],
      otherLifts: List[Planter[?, ?, ?]],
      originalEntityLifts: List[InjectableEagerPlanter[_, _, _]],
      liftingPlaceholder: Int => String,
      emptyContainsToken: Token => Token,
      batchingBehavior: BatchingBehavior,
      extractBehavior: BatchExtractBehavior,
      traceConfig: TraceConfig
  ) {
    def apply(): List[(String, List[(PrepareRow, Session) => (List[Any], PrepareRow)])] =
      batchingBehavior match
        // If we have MultiRowsPerBatch behavior and we are instructed to concatenate multiple rows together (i.e. entitiesPerQuery > 1)
        case BatchingBehavior.MultiRowsPerBatch(entitiesPerQuery) if (entitiesPerQuery > 1) =>
          val validations =
            for {
              _ <- validateConcatenatedIterationPossible(query, entitiesPerQuery)
              _ <- validateIdiomSupportsConcatenatedIteration(idiom, extractBehavior)
            } yield ()

          validations match
            case Left(msg) =>
              logger.underlying.warn(msg)
              singleRowIteration()
            case Right(_) =>
              concatenatedRowIteration(entitiesPerQuery)

        case _ =>
          singleRowIteration()

    // NOTE: First we can particularize for every query as explained below.
    // If needed, at some point we can optimize and have just two query particularizations:
    // if (entitiesCount <= batchSize)
    //    batch(single)(entitiesSize%batchSize)
    // else
    //    insert-batch(notLast)(batchSize) + insert-batch(last)(entitiesSize%batchSize)
    //
    // In general we get the VALUES-clause lifts for all rows we want to insert in this query
    // E.g. in a batch-set that looks like (assuming batchSize=3 i.e. each VALUES-clause is a row):
    // (note that the WHERE clause is not technically possible insertions but can happen with UPDATEs that CAN have WHERE clauses)
    //   Query1: INSERT INTO Person (name, age) VALUES ('Joe', 22), ('Jack', 33), ('Jill', 44) WHERE something=liftedValue
    //   Query2: INSERT INTO Person (name, age) VALUES ('LastGuy', 55)                         WHERE something=liftedValue
    // We will have the groups:
    // (using [stuff, stuff] as list syntax), l:stuff for lift(stuff) a.k.a. Planter(stuff), sing: means singleQueryEntities
    //   Query1: sing:[ SingleEntityLifts([l:'Joe', l:22]), SingleEntityLifts([l:'Jack', l:33]), SingleEntityLifts([l:'Jill', l:44]) ], otherLifts:[liftedValue]
    //   Query2: sing:[ SingleEntityLifts([l:'LastGuy', l:55]) ],                                                                       otherLifts:[liftedValue]
    //
    // Another possibility (This is actually the 1st clause in the code arrangement below) is that there are only 3 rows in total,
    // in that case there will only be one query:
    //   Query1: INSERT INTO Person (name, age) VALUES ('Joe', 22), ('Jack', 33), ('Jill', 44) WHERE something=liftedValue
    // We will have just one group:
    //   Query1: sing:[ SingleEntityLifts([l:'Joe', l:22]), SingleEntityLifts([l:'Jack', l:33]), SingleEntityLifts([l:'Jill', l:44]) ], otherLifts:[liftedValue]
    def concatenatedRowIteration(numEntitiesPerQuery: Int): List[(String, List[(PrepareRow, Session) => (List[Any], PrepareRow)])] = {
      val totalEntityCount = perRowLifts.length
      val templateOfLifts = originalEntityLifts ++ otherLifts

      // if (entitiesCount <= batchSize)
      //   batch(single)(entitiesSize%batchSize)
      if (totalEntityCount <= numEntitiesPerQuery) {
        val (singleGroupQuery, liftsOrderer) = Particularize.Dynamic(query, templateOfLifts, liftingPlaceholder, emptyContainsToken, /*valueClauseRepeats*/ totalEntityCount)(traceConfig)

        // Since the entire query will fit into one bach, we don't need to subdivide the batches
        // just make prepares based on all of the lifts
        val orderedLifts = liftsOrderer.orderLifts(perRowLifts, otherLifts)
        val prepares =
          (row: PrepareRow, session: Session) => {
            LiftsExtractor.apply[PrepareRow, Session](orderedLifts, row, session)
          }

        // Output here is a single Query and Single prepares list (although the type is )
        List((singleGroupQuery, List(prepares)))
      }
      // else (entitiesCount > batchSize)
      //    insert-batch(notLast)(batchSize) + insert-batch(last)(entitiesSize % batchSize)
      // In other words, if we are doing batchSize=1000 i.e. 1000 rows per query and we have 2200 rows, make three batch groups
      //   The 1st and 2nd that insert 1000 rows each, that's the queryForMostGroups
      //   The 3rd which only inserts 200 i.e. 2200 % batchSize
      else {
        val (anteriorQuery, anteriorLiftsOrderer) = Particularize.Dynamic(query, templateOfLifts, liftingPlaceholder, emptyContainsToken, numEntitiesPerQuery)(traceConfig)
        val lastQueryEntityCount = totalEntityCount % numEntitiesPerQuery
        val (lastQuery, lastLiftsOrderer) = Particularize.Dynamic(query, templateOfLifts, liftingPlaceholder, emptyContainsToken, lastQueryEntityCount)(traceConfig)
        // println(s"Most Queries: ${numEntitiesPerQuery} Entities, Last Query: ${lastQueryEntityCount} Entities")

        // Say you have `liftQuery(A,B,C,D,E).foreach(...)` and numEntitiesPerQuery:=2 you need to do the following:
        // (where by lift(As) I mean lift all the columns of A)
        //   Prepare(INSERT ... VALUES (?, ?)) run-on (lift(As), lift(Bs)), then run-on (lift(Cs), lift(Ds))
        // Then the one final query:
        //   Prepare(INSERT ... VALUES (?)) run-on (lift(Es))
        // Since perRowLifts.grouped(numEntitiesPerQuery).toList this case would be:
        //   List(List(A, B), List(C, D), List(E)) so we need to drop the last List(E) i.e. on the dropRight(1)
        //
        // If we have fewer entities to insert however (i.e. numEntitiesPerQuery > 2 e.g. just List(List(A))) then
        // we just take that and do not drop anything.
        val groupedLifts = perRowLifts.grouped(numEntitiesPerQuery).toList
        val entitiesInQueries =
          if (lastQueryEntityCount > 0)
            groupedLifts.dropRight(1)
          else
            groupedLifts

        val anteriorPrepares =
          entitiesInQueries.map { entitiesInOneQuery =>
            // So firstly we need to combine the `sing` and `otherLifts` groups (see comment above for more explanation)
            val liftsInThisGroup = anteriorLiftsOrderer.orderLifts(entitiesInOneQuery, otherLifts)
            (row: PrepareRow, session: Session) =>
              LiftsExtractor.apply[PrepareRow, Session](liftsInThisGroup, row, session)
          }
        val lastPrepare = {
          val lastEntities = groupedLifts.last
          val liftsInThisGroup = lastLiftsOrderer.orderLifts(lastEntities, otherLifts)
          (row: PrepareRow, session: Session) =>
            LiftsExtractor.apply[PrepareRow, Session](liftsInThisGroup, row, session)
        }
        List(
          (anteriorQuery, anteriorPrepares)
        ) ++ (
          if (lastQueryEntityCount > 0)
            List((lastQuery, List(lastPrepare)))
          else
            Nil
        )
      }
    }

    private def validateIdiomSupportsConcatenatedIteration(idiom: Idiom, extractBehavior: BatchExtractBehavior) =
      extractBehavior match
        case ExtractBehavior.Skip =>
          validateIdiomSupportsConcatenatedIterationNormal(idiom)
        case ExtractBehavior.ExtractWithReturnAction =>
          validateIdiomSupportsConcatenatedIterationReturning(idiom)

    private def validateIdiomSupportsConcatenatedIterationNormal(idiom: Idiom) = {
      import io.getquill.context.InsertValueMulti
      val hasCapability =
        if (idiom.isInstanceOf[IdiomInsertValueCapability])
          idiom.asInstanceOf[IdiomInsertValueCapability].idiomInsertValuesCapability == InsertValueMulti
        else
          false

      if (hasCapability)
        Right(())
      else
        Left(
          s"""|The dialect ${idiom.getClass.getName} does not support inserting multiple rows-per-batch (e.g. it cannot support multiple VALUES clauses).
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server, Sqlite).
            |Falling back to the regular single-row-per-batch insert behavior.
            |""".stripMargin
        )
    }

    private def validateIdiomSupportsConcatenatedIterationReturning(idiom: Idiom) = {
      import io.getquill.context.InsertValueMulti
      val hasCapability =
        if (idiom.isInstanceOf[IdiomInsertReturningValueCapability])
          idiom.asInstanceOf[IdiomInsertReturningValueCapability].idiomInsertReturningValuesCapability == InsertReturningValueMulti
        else
          false

      if (hasCapability)
        Right(())
      else
        Left(
          s"""|The dialect ${idiom.getClass.getName} does not support inserting multiple rows-per-batch (e.g. it cannot support multiple VALUES clauses)
            |when batching with query-returns and/or generated-keys.
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server).
            |Falling back to the regular single-row-per-batch insert-returning behavior.
            |""".stripMargin
        )
    }

    private def validateConcatenatedIterationPossible(query: Unparticular.Query, entitiesPerQuery: Int) = {
      import io.getquill.idiom._
      def valueClauseExistsIn(token: Token): Boolean =
        token match
          case _: ValuesClauseToken           => true
          case _: StringToken                 => false
          case _: ScalarTagToken              => false
          case _: QuotationTagToken           => false
          case _: ScalarLiftToken             => false
          case Statement(tokens: List[Token]) => tokens.exists(valueClauseExistsIn(_) == true)
          case SetContainsToken(a: Token, op: Token, b: Token) =>
            valueClauseExistsIn(a) || valueClauseExistsIn(op) || valueClauseExistsIn(b)

      if (valueClauseExistsIn(query.realQuery))
        Right(())
      else
        Left(
          s"""|Cannot insert multiple (i.e. ${entitiesPerQuery}) rows per-batch-query since the query ${query.basicQuery} has no VALUES clause.
            |Currently this functionality is only supported for INSERT queries for select databases (Postgres, H2, SQL Server, Sqlite).
            |Falling back to the regular single-row-per-batch insert behavior.
            |""".stripMargin
        )
    }

    def singleRowIteration(): List[(String, List[(PrepareRow, Session) => (List[Any], PrepareRow)])] = {
      val numEntitiesInAllQueries = 1
      // Since every batch consists of one row inserted, can use the original InjectableEagerPlanter here to Particularize (i.e. insert the right number of '?' into) the query
      val liftsInAllGroups = originalEntityLifts ++ otherLifts
      val (allGroupsQuery, liftsOrderer) = Particularize.Dynamic(query, liftsInAllGroups, liftingPlaceholder, emptyContainsToken, numEntitiesInAllQueries)(traceConfig)
      val prepares =
        perRowLifts.map {
          liftsInThisGroup =>
            val orderedLifts = liftsOrderer.orderLifts(List(liftsInThisGroup), otherLifts)
            {
              (row: PrepareRow, session: Session) =>
                LiftsExtractor.apply[PrepareRow, Session](orderedLifts, row, session)
            }
        }
      List((allGroupsQuery, prepares))
    }

  }
}
