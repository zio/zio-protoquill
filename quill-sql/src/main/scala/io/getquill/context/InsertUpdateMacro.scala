
package io.getquill.context


import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadModule
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.ParserFactory
import io.getquill.generic.ElaborateStructure
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, Update => AUpdate, _ }
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.metaprog.Extractors._
import io.getquill.Quoted
import io.getquill.EntityQuery
import io.getquill.QuotationVase
import io.getquill.InsertMeta
import io.getquill.UpdateMeta
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.PlanterExpr
import io.getquill.Planter
import io.getquill.Insert
import io.getquill.Update
import io.getquill.util.Format
import io.getquill.generic.ElaborationSide
import io.getquill.metaprog.SummonParser
import _root_.io.getquill.ActionReturning

/**
 * TODO Right now this is just insert but we can easily extend to update and delete
 *
 * The function call that regularly drives query insertion is
 * {code}
 * query[T].insert(_.field1 -> value, _.field2 -> value, etc...)
 * {code}
 * Let's call this the field-insertion api.
 *
 * This macro essentially takes an insert of the form `query[T].insert(T(...))` and converts into the former form.
 *
 * Once we've parsed an insert e.g. `query[Person].insert(Person("Joe", "Bloggs"))` we then need to synthesize
 * the insertions that this would represent e.g. `query[Person].insert(_.firstName -> "Joe", _.lastName -> "Bloggs")`
 *
 * Each function of field-insertion API basically takes the form
 * {code} (v) => vAssignmentProperty -> assignmentValue (on the AST) {code}
 *
 * Let's take a look at a slighly more complex example
 * Given:
 * {code}
 *  case class Person(name: String, age: Option[Age]); Age(value: Int)
 *  quote { query[Person].insert(Person("Joe", Age(345))) }
 * {code}
 *
 * This expands out into a series of statements which will be parsed to AST assignments
 * This: `(v: Person) => v.name -> (v:Person).name`
 * Will be parsed into this:
 * {code} Assignment(Id(v), Prop(Id(v), name), Constant("Joe")) {code}
 *
 * This: `(v: Person) => v.age.map(v => v.value) -> Option(v:Age).map(v => v.value)`
 * Will be parsed into this:
 * {code}
 *   Assignment(Id(v),
 *     OptionTableMap(Prop(Id(v), age), Id(v), Prop(Id(v), value))
 *     OptionTableMap(OptionApply(CaseClass(value=345)), Id(v), Prop(Id(v), value))
 *   )
 * {code}
 *
 * The end result of this synthesis is a series of assignments for an insert for the given entity.
 *
 * Another possiblity is that the entity is lifted:
 * {code}
 *  case class Person(name: String, age: Option[Age]); Age(value: Int)
 *  quote { query[Person].insert(lift(Person("Joe", Age(345)))) }
 * {code}
 * TODO Finish doc
 *
 * Note that as a result of the way this is implemented, if either the InsertMeta or the SchemaMeta is not
 * inline, the entire resulting query will not be inline since they both will be summoned and used in the
 * resulting expressions. It might be useful to introduce a configuration parameter to ignore non-inline InsertMetas
 * or non-inline SchemaMetas. Or maybe this could even be an annotation.
 */
object InsertUpdateMacro {
  // Using this ident since it is replaced in cleanIdent so user will not create something conflicting with it
  private[getquill] val VIdent = AIdent("_$V", Quat.Generic)

  def foo = {
    Tuple2
  }

  object DynamicUtil {
    def retrieveAssignmentTuple(quoted: Quoted[_]): Set[Ast] =
      quoted.ast match
        case Tuple(values) if (values.forall(_.isInstanceOf[Property])) => values.toSet
        case other => throw new IllegalArgumentException(s"Invalid values in InsertMeta: ${other}. An InsertMeta AST must be a tuple of Property elements.")
  }

  enum SummonState[+T]:
    case Static(value: T) extends SummonState[T]
    case Dynamic(uid: String, quotation: Expr[Quoted[Any]]) extends SummonState[Nothing]

  /**
   * Perform the pipeline of creating an insert statement. The 'insertee' is the case class on which the SQL insert
   * statement is based. The schema is based on the EntityQuery which could potentially be an unquoted QuerySchema.
   */
  class Pipeline[T: Type, A[T] <: Insert[T] | Update[T]: Type](using override val qctx: Quotes) extends QuatMaking with QuatMakingBase(using qctx):
    import quotes.reflect._
    import io.getquill.util.Messages.qprint


    case class InserteeSchema(schemaRaw: Expr[EntityQuery[T]]):
      private def plainEntity: Entity =
        val entityName = TypeRepr.of[T].classSymbol.get.name
        Entity(entityName, List(), InferQuat.of[T].probit)

      def summon: SummonState[Ast] =
        val schema = schemaRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]
        UntypeExpr(schema) match
          // Case 1: query[Person].insert(...)
          // the schemaRaw part is {query[Person]} which is a plain entity query (as returned from QueryMacro)
          case '{ EntityQuery[t] } => SummonState.Static(plainEntity)
          // Case 2: querySchema[Person](...).insert(...)
          // there are query schemas involved i.e. the {querySchema[Person]} part is a QuotationLotExpr.Unquoted that has been spliced in
          // also if there is an implicit/given schemaMeta this case will be hit (because if there is a schemaMeta,
          // the query macro would have spliced it into the code already).
          // TODO when using querySchema directly this doesn't work. Need to test that out and make it work
          case QuotationLotExpr.Unquoted(unquotation) =>
            unquotation match
              // The {querySchema[Person]} part is static (i.e. fully known at compile-time)
              case Uprootable.Ast(ast) =>
                Unlifter(ast) match
                  case ent: Entity => SummonState.Static(ent)
                  case other       => report.throwError(s"Unlifted insertion Entity '${qprint(other).plainText}' is not a Query.")
              // The {querySchema[Person]} is dynamic (i.e. not fully known at compile-time)
              case Pluckable(uid, quotation, _) =>
                SummonState.Dynamic(uid, quotation)
              case _ =>
                report.throwError(s"Quotation Lot of InsertMeta either pluckable or uprootable from: '${unquotation}'")

          // parse this
          case '{ ($q: EntityQuery[t]) } =>
            val parserFactory = SummonParser()
            val ast = parserFactory.apply.seal.apply(q)
            SummonState.Static(ast)

          case _ =>
            report.throwError(s"Cannot process illegal insert meta: ${Format.Expr(schema)}")
              // TODO Make an option to ignore dynamic entity schemas and return the plain entity?
              //println("WARNING: Only inline schema-metas are supported for insertions so far. Falling back to a plain entity.")
              //plainEntity
    end InserteeSchema

    enum MacroType:
      case Insert
      case Update
    object MacroType:
      def asString = ofThis().toString
      def ofThis() =
        if (TypeRepr.of[A] <:< TypeRepr.of[Insert])
          MacroType.Insert
        else if (TypeRepr.of[A] <:< TypeRepr.of[Update])
          MacroType.Update
        else
          report.throwError(s"Invalid macro action type ${io.getquill.util.Format.TypeOf[A[Any]]} must be either Insert or Update")
      def summonMetaOfThis() =
        ofThis() match
          case MacroType.Insert => Expr.summon[InsertMeta[T]]
          case MacroType.Update => Expr.summon[UpdateMeta[T]]


    object IgnoredColumns:
      def summon: SummonState[Set[Ast]] =
        // If someone has defined a: given meta: InsertMeta[Person] = insertMeta[Person](_.id) or UpdateMeta[Person] = updateMeta[Person](_.id)
        MacroType.summonMetaOfThis() match
          case Some(actionMeta) =>
            QuotationLotExpr(actionMeta.asTerm.underlyingArgument.asExpr) match
              // if the meta is inline i.e. 'inline given meta: InsertMeta[Person] = ...' (or UpdateMeta[Person])
              case Uprootable.Ast(ast) =>
                Unlifter(ast) match
                  case Tuple(values) if (values.forall(_.isInstanceOf[Property])) =>
                    SummonState.Static(values.toSet)
                  case other =>
                    report.throwError(s"Invalid values in ${Format.TypeRepr(actionMeta.asTerm.tpe)}: ${other}. An ${Format.TypeRepr(actionMeta.asTerm.tpe)} AST must be a tuple of Property elements.")
              // if the meta is not inline
              case Pluckable(uid, quotation, _) =>
                SummonState.Dynamic(uid, quotation)
              case _ =>
                report.throwError(s"The ${MacroType.asString}Meta form is invalid. It is Pointable: ${io.getquill.util.Format.Expr(actionMeta)}. It must be either Uprootable or Pluckable i.e. it has at least a UID that can be identified.")
                // TODO Configuration to ignore dynamic insert metas?
                //println("WARNING: Only inline insert-metas are supported for insertions so far. Falling back to a insertion of all fields.")
          case None =>
            SummonState.Static(Set.empty)

    /**
     * Inserted object
     * can either be static: query[Person].insert(Person("Joe", "Bloggs"))
     * or it can be lifted:  query[Person].insert(lift(Person("Joe", "Bloggs")))
     *
     * In the later case, it will become:
     * {{
     *   //Assuming x := Person("Joe", "Bloggs")
     *   CaseClassLift(
     *    Quoted(ast: CaseClass(name -> lift(idA)), ...), lifts: List(EagerLift(x.name, idA), ...))
     *   )
     * }}
     *
     * For batch queries liftQuery(people).foreach(p => query[Person].insert(p))
     * it will be just the ast Ident("p")
     */
    def parseInsertee(insertee: Expr[Any]): CaseClass | AIdent = {
      insertee match
        // The case: query[Person].insert(lift(Person("Joe", "Bloggs")))
        case QuotationLotExpr(exprType) =>
          exprType match
            // If clause is uprootable, pull it out. Note that any lifts inside don't need to be extracted here
            // since they will be extracted later in ExtractLifts
            case Uprootable.Ast(astExpr) =>
              val ast = Unlifter(astExpr)
              if (!ast.isInstanceOf[CaseClass])
                report.throwError(s"The lifted insertion element needs to be parsed as a Ast CaseClass but it is: ${ast}")
              ast.asInstanceOf[CaseClass]
            case _ =>
              report.throwError("Cannot uproot lifted element. A lifted Insert element e.g. query[T].insert(lift(element)) must be lifted directly inside the lift clause.")
        // Otherwise the inserted element (i.e. the insertee) is static and should be parsed as an ordinary case class
        // i.e. the case query[Person].insert(Person("Joe", "Bloggs")) (or the batch case)
        case _ =>
          parseStaticInsertee(insertee)
    }

    /**
     * Parse the input to of query[Person].insert(Person("Joe", "Bloggs")) into CaseClass(firstName="Joe",lastName="Bloggs")
     */
    def parseStaticInsertee(insertee: Expr[_]): CaseClass | AIdent = {
      val parserFactory = SummonParser()
      val rawAst = parserFactory.apply.seal.apply(insertee)
      val ast = BetaReduction(rawAst)
      ast match
        case cc: CaseClass => cc
        case id: AIdent => id
        case _ => report.throwError(s"Parsed Insert Macro AST is not a Case Class: ${qprint(ast).plainText} (or a batch-query Ident)")
    }

    /**
     * Actually the same as deduceAssignmentsFromCaseClass, but I decided to write
     * a separate function and comment it extensively since the logic is not simple to
     * extrapolate.
     * This function creates a series of assignments
     * of a elaborated product. However, each assignment just assigns to the identifier
     * which will be plugged in (i.e. BetaReduced) once the Ident is actually substituted.
     * E.g. if we have something like this: `val ip = quote { (p: Person) => query[Person].insert(p) }`
     * and then later: `run(ip(lift(Person("Joe",123))))` then the assignments list is just based
     * on the `p` identifier of the `ip` quoted function i.e:
     * `(v:Person) => v.firstName -> p.firstName` this is achived by doing
     * BetaReduce(v.firstName, v -> p). Later on when `ip(lift(Person("Joe",123)))`
     * happens the `CaseClass(firstName -> lift(...), age -> lift(...))` comes in and
     * all the right values are plugged in correctly.
     */
    def deduceAssignmentsFromIdent(insertee: AIdent) = {
      val expansionList = ElaborateStructure.ofProductType[T](VIdent.name, ElaborationSide.Encoding) // Elaboration side is Encoding since this is for an entity being inserted
      def mapping(path: Ast) =
        val reduction = BetaReduction(path, VIdent -> insertee)
        Assignment(VIdent, path, reduction)

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    def deduceAssignmentsFromCaseClass(insertee: CaseClass) = {
      // Expand into a AST
      // T:Person(name:Str, age:Option[Age]) Age(value: Int) -> Ast: List(v.name, v.age.map(v => v.value))
      val expansionList = ElaborateStructure.ofProductType[T](VIdent.name, ElaborationSide.Encoding)  // Elaboration side is Encoding since this is for an entity being inserted

      // Now synthesize (v) => vAssignmentProperty -> assignmentValue
      // e.g. (v:Person) => v.firstName -> "Joe"
      // TODO, Ast should always be a case class (maybe a tuple?) should verify that
      def mapping(path: Ast) =
        val reduction = BetaReduction(path, VIdent -> insertee)
        Assignment(VIdent, path, reduction)

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    /**
     * Get assignments from an entity and then either exclude or include them
     * either statically or dynamically.
     */
    def processAssignmentsAndExclusions(assignmentsOfEntity: List[io.getquill.ast.Assignment]): Expr[List[io.getquill.ast.Assignment]] =
      IgnoredColumns.summon match
        // If we have assignment-exclusions during compile time
        case SummonState.Static(exclusions) =>
          // process which assignments to exclude and take them out
          val remainingAssignments = assignmentsOfEntity.filterNot(asi => exclusions.contains(asi.property))
          // Then lift the remaining assignments
          val liftedAssignmentsOfEntity = Expr.ofList(remainingAssignments.map(asi => Lifter.NotSerializing.assignment(asi)))
          // ... and return them in lifted form
          liftedAssignmentsOfEntity
        // If we have assignment-exclusions that can only be accessed during runtime
        case SummonState.Dynamic(uid, quotation) =>
          // Pull out the exclusions from the quotation
          val exclusions = '{ DynamicUtil.retrieveAssignmentTuple($quotation) }
          // Lift ALL the assignments of the entity
          val allAssignmentsLifted = Expr.ofList(assignmentsOfEntity.map(ast => Lifter.NotSerializing.assignment(ast)))
          // Create a statement that represents the filtered assignments during runtime
          val liftedFilteredAssignments = '{ $allAssignmentsLifted.filterNot(asi => $exclusions.contains(asi.property)) }
          // ... and return the filtered assignments
          liftedFilteredAssignments


    /**
     * Note that the only reason Parser is needed here is to pass it into parseInsertee.
     * The batch pipeline driven by createFromPremade currently doesn't need it.
     */
    def apply(schemaRaw: Expr[EntityQuery[T]], inserteeRaw: Expr[T]) = {
      val insertee = inserteeRaw.asTerm.underlyingArgument.asExpr
      val assignmentOfEntity =
        parseInsertee(insertee) match
          // if it is a CaseClass we either have a static thing e.g. query[Person].insert(Person("Joe", 123))
          // or we have a lifted thing e.g. query[Person].insert(lift(Person("Joe", 123)))
          // so we just process it based on what kind of pattern we encounter
          case astCaseClass: CaseClass => deduceAssignmentsFromCaseClass(astCaseClass)
          // if it is a Ident, then we know we have a batch query i.e. liftQuery(people).foreach(p => query[Person].insert(p))
          // we want to re-syntheize this as a lifted thing i.e. liftQuery(people).foreach(p => query[Person].insert(lift(p)))
          // and then reprocess the contents.
          // We don't want to do that here thought because we don't have the PrepareRow
          // so we can't lift content here into planters. Instead this is done in the BatchQueryExecution pipeline
          case astIdent: AIdent => deduceAssignmentsFromIdent(astIdent)

      // Insertion could have lifts and quotes inside, need to extract those.
      // E.g. it can be 'query[Person].insert(lift(Person("Joe",123)))'' which becomes Quoted(CaseClass(name -> lift(x), age -> lift(y), List(ScalarLift("Joe", x), ScalarLift(123, y)), Nil).
      // (In some cases, maybe even the runtimeQuotes position could contain things)
      // However, the insertee itself must always be available statically (i.e. it must be a Uprootable Quotation)
      val (lifts, pluckedUnquotes) = ExtractLifts(inserteeRaw)

      val quotation =
        createQuotation(
          InserteeSchema(schemaRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]).summon,
          assignmentOfEntity, lifts, pluckedUnquotes
        )
      UnquoteMacro(quotation)
    }

    /**
     * Create a static or dynamic quotation based on the state. Wrap the expr using some additional functions if we need to.
     * This is used for the createFromPremade if we need to wrap it into insertReturning which is used for batch-returning query execution.
     */
    def createQuotation(summonState: SummonState[Ast], assignmentOfEntity: List[Assignment], lifts: List[Expr[Planter[?, ?, ?]]], pluckedUnquotes: List[Expr[QuotationVase]]) = {
      //println("******************* TOP OF APPLY **************")
      // Processed Assignments AST plus any lifts that may have come from the assignments AST themsevles.
      // That is usually the case when
      val assignmentsAst = processAssignmentsAndExclusions(assignmentOfEntity)

      // TODO where if there is a schemaMeta? Need to use that to create the entity
      summonState match
        // If we can get a static entity back
        case SummonState.Static(entity) =>
          // Lift it into an `Insert` ast, put that into a `quotation`, then return that `quotation.unquote` i.e. ready to splice into the quotation from which this `.insert` macro has been called
          val action = MacroType.ofThis() match
              case MacroType.Insert =>
                '{ AInsert(${Lifter(entity)}, ${assignmentsAst}) }
              case MacroType.Update =>
                '{ AUpdate(${Lifter(entity)}, ${assignmentsAst}) }

          val quotation = '{ Quoted[A[T]](${action}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }
          // Unquote the quotation and return
          quotation

        // If we get a dynamic entity back
        // e.g. entityQuotation is 'querySchema[Person](...)' which is not inline
        case SummonState.Dynamic(uid, entityQuotation) =>
          // Need to create a ScalarTag representing a splicing of the entity (then going to add the actual thing into a QuotationVase and add to the pluckedUnquotes)

          val action = MacroType.ofThis() match
              case MacroType.Insert =>
                '{ AInsert(QuotationTag(${Expr(uid)}), ${assignmentsAst}) }
              case MacroType.Update =>
                '{ AUpdate(QuotationTag(${Expr(uid)}), ${assignmentsAst}) }

          // Create the QuotationVase in which this dynamic quotation will go
          val runtimeQuote = '{ QuotationVase($entityQuotation, ${Expr(uid)}) }
          // Then create the quotation, adding the new runtimeQuote to the list of pluckedUnquotes
          val quotation = '{ Quoted[A[T]](${action}, ${Expr.ofList(lifts)}, $runtimeQuote +: ${Expr.ofList(pluckedUnquotes)}) }
          // Unquote the quotation and return
          quotation

      // use the quoation macro to parse the value into a class expression
      // use that with (v) => (v) -> (class-value) to create a quoation
      // incorporate that into a new quotation, use the generated quotation's lifts and runtime lifts
      // the output value
      // use the Unquote macro to take it back to an 'Insert[T]'
    }

  end Pipeline

  def apply[T: Type, A[T] <: Insert[T] | Update[T]: Type](entityRaw: Expr[EntityQuery[T]], bodyRaw: Expr[T])(using Quotes): Expr[A[T]] =
    new Pipeline[T, A]().apply(entityRaw, bodyRaw)
}
