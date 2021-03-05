
package io.getquill.context

import io.getquill._
import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.ParserFactory
import io.getquill.generic.ElaborateStructure
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, _ }
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.metaprog.QuotationLotExpr
import io.getquill.metaprog.QuotationLotExpr._
import io.getquill.metaprog.Extractors
import io.getquill.Quoted
import io.getquill.QuotationVase
import io.getquill.InsertMeta
import io.getquill.quat.QuatMaking
import io.getquill.quat.QuatMakingBase
import io.getquill.quat.Quat
import io.getquill.metaprog.PlanterExpr

/**
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
 */
object InsertMacro {
  private[getquill] val VIdent = AIdent("v", Quat.Generic)

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
   * statement is based. The schema is based on the EntityQuery which could potentially be an unquoted SchemaMeta.
   */
  class Pipeline[T: Type, Parser <: ParserFactory: Type](schemaRaw: Expr[EntityQuery[T]], inserteeRaw: Expr[T])(using override val qctx: Quotes) extends Extractors with QuatMaking, QuatMakingBase(using qctx):
    import quotes.reflect._
    import io.getquill.util.Messages.qprint


    object EntitySchema:
      private def plainEntity: Entity =
        val entityName = TypeRepr.of[T].classSymbol.get.name
        Entity(entityName, List(), InferQuat.of[T].probit)

      def summon: SummonState[Entity] =
        val schema = schemaRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]
        UntypeExpr(schema) match 
          // TODO What if it is an qnuote container a querySchema? Will that just work normally? getting an Entity object
          // If it is a plain entity query (as returned from QueryMacro)
          case '{ EntityQuery[t] } => SummonState.Static(plainEntity)
          // If there are query schemas involved i.e. a QuotationLotExpr.Unquoted has been spliced in
          case QuotationLotExpr.Unquoted(unquotation) => unquotation match
            case Uprootable(_, ast, _, _, _, _) => 
              Unlifter(ast) match
                case ent: Entity => SummonState.Static(ent)
                case other => report.throwError(s"Unlifted insertion Entity '${qprint(other).plainText}' is not a Query.")
            case Pluckable(uid, quotation, _) =>
              SummonState.Dynamic(uid, quotation)
            case _ => report.throwError(s"Quotation Lot of InsertMeta either pluckable or uprootable from: '${unquotation}'")
          
          case _ => report.throwError(s"Cannot process illegal insert meta: ${schema}")

              // TODO Make an option to ignore dynamic entity schemas and return the plain entity?
              //println("WARNING: Only inline schema-metas are supported for insertions so far. Falling back to a plain entity.")
              //plainEntity

    
    object InsertExclusions:
      def summon: SummonState[Set[Ast]] =
        Expr.summon[InsertMeta[T]] match
          case Some(insertMeta) =>
            QuotationLotExpr(insertMeta.asTerm.underlyingArgument.asExpr) match
              case Uprootable(_, ast, _, _, _, _) =>
                //println(s"***************** Found an uprootable ast: ${ast.show} *****************")
                Unlifter(ast) match
                  case Tuple(values) if (values.forall(_.isInstanceOf[Property])) =>
                    SummonState.Static(values.toSet)
                  case other => 
                    report.throwError(s"Invalid values in InsertMeta: ${other}. An InsertMeta AST must be a tuple of Property elements.")
              case Pluckable(uid, quotation, _) =>
                SummonState.Dynamic(uid, quotation)
              // TODO Improve this error
              case _ => report.throwError("Invalid form, cannot be pointable")

                // TODO Configuration to ignore dynamic insert metas?
                //println("WARNING: Only inline insert-metas are supported for insertions so far. Falling back to a insertion of all fields.")
                //
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
     */
    def parseInsertee(): CaseClass = {
      val insertee = inserteeRaw.asTerm.underlyingArgument.asExpr
      insertee match
        case QuotationLotExpr(exprType) =>
          exprType match
            // If clause is uprootable, pull it out. Note that any lifts inside don't need to be extracted here 
            // since they will be extracted later in ExtractLifts
            case Uprootable(_, astExpr, _, _, _, _) => 
              val ast = Unlifter(astExpr)
              if (!ast.isInstanceOf[CaseClass])
                report.throwError(s"The lifted insertion element needs to be parsed as a Ast CaseClass but it is: ${ast}")
              ast.asInstanceOf[CaseClass]
            case _ => 
              report.throwError("Cannot uproot lifted element. A lifted Insert element e.g. query[T].insert(lift(element)) must be lifted directly inside the lift clause.")
        // Otherwise the inserted element (i.e. the insertee) is static and should be parsed as an ordinary case class
        case _ =>
          parseStaticInsertee(insertee)
    }

    /** 
     * Parse the input to of query[Person].insert(Person("Joe", "Bloggs")) into CaseClass(firstName="Joe",lastName="Bloggs") 
     */
    def parseStaticInsertee(insertee: Expr[_]): CaseClass = {  
      val parserFactory = LoadObject[Parser].get
      val rawAst = parserFactory.apply.seal.apply(insertee)
      val ast = BetaReduction(rawAst)
      // TODO Implement dynamic pipeline for schema meta
      ast match
        case cc: CaseClass => cc
        case _ => report.throwError(s"Parsed Insert Macro AST is not a Case Class: ${qprint(ast).plainText}")  
    }

    def deduceAssignmentsFrom(insertee: CaseClass) = {
      // Expand into a AST
      // T:Person(name:Str, age:Option[Age]) Age(value: Int) -> Ast: List(v.name, v.age.map(v => v.value))
      val expansionList = ElaborateStructure.ofProductType[T](VIdent.name)

      // Now synthesize (v) => vAssignmentProperty -> assignmentValue
      // e.g. (p:Person) => p.firstName -> "Joe"
      // TODO, Ast should always be a case class (maybe a tuple?) should verify that
      def mapping(path: Ast) =
        val reduction = BetaReduction(path, VIdent -> insertee)
        Assignment(VIdent, path, reduction)

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    def deduceAssignments() =
      val astCaseClass = parseInsertee()
      deduceAssignmentsFrom(astCaseClass)

    /** 
     * Get assignments from an entity and then either exclude or include them
     * either statically or dynamically.
     */
    def processAssignmentExclusions(): Expr[List[io.getquill.ast.Assignment]] =
      val assignmentsOfEntity = deduceAssignments()
      InsertExclusions.summon match
        // If we have assignment-exclusions during compile time
        case SummonState.Static(exclusions) =>
          // process which assignments to exclude and take them out
          val remainingAssignments = assignmentsOfEntity.filterNot(asi => exclusions.contains(asi.property))
          // Then lift the remaining assignments
          val liftedAssignmentsOfEntity = Expr.ofList(remainingAssignments.map(asi => Lifter.assignment(asi)))
          // ... and return them in lifted form
          liftedAssignmentsOfEntity
        // If we have assignment-exclusions that can only be accessed during runtime
        case SummonState.Dynamic(uid, quotation) =>
          // Pull out the exclusions from the quotation
          val exclusions = '{ DynamicUtil.retrieveAssignmentTuple($quotation) }
          // Lift ALL the assignments of the entity
          val allAssignmentsLifted = Expr.ofList(assignmentsOfEntity.map(ast => Lifter.assignment(ast)))
          // Create a statement that represents the filtered assignments during runtime
          val liftedFilteredAssignments = '{ $allAssignmentsLifted.filterNot(asi => $exclusions.contains(asi.property)) }
          // ... and return the filtered assignments
          liftedFilteredAssignments

    def apply = {

      //println("******************* TOP OF APPLY **************")
      // Processed Assignments AST plus any lifts that may have come from the assignments AST themsevles.
      // That is usually the case when 
      val assignmentsAst = processAssignmentExclusions()

      // Insertion could have lifts and quotes, need to extract those. Note that the Insertee must always
      // be available dynamically (i.e. it must be a Uprootable Quotation)
      val (lifts, pluckedUnquotes) = ExtractLifts(inserteeRaw)

      // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)
      // TODO if there is a schemaMeta need to use that to create the entity
      val unquotation =
        EntitySchema.summon match
          // If we can get a static entity back
          case SummonState.Static(entity) =>
            // Lift it into an `Insert` ast, put that into a `quotation`, then return that `quotation.unquote` i.e. ready to splice into the quotation from which this `.insert` macro has been called
            val insert = '{ AInsert(${Lifter.entity(entity)}, ${assignmentsAst}) }
            val quotation = '{ Quoted[Insert[T]](${insert}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }
            // Unquote the quotation and return
            UnquoteMacro(quotation)

          // If we get a dynamic entity back
          case SummonState.Dynamic(uid, entityQuotation) =>
            // Need to create a ScalarTag representing a splicing of the entity (then going to add the actual thing into a QuotationVase and add to the pluckedUnquotes)
            val insert = '{ AInsert(QuotationTag(${Expr(uid)}), ${assignmentsAst}) }
            // Create the QuotationVase in which this dynamic quotation will go
            val runtimeQuote = '{ QuotationVase($entityQuotation, ${Expr(uid)}) }
            // Then create the quotation, adding the new runtimeQuote to the list of pluckedUnquotes
            val quotation = '{ Quoted[Insert[T]](${insert}, ${Expr.ofList(lifts)}, $runtimeQuote +: ${Expr.ofList(pluckedUnquotes)}) }
            // Unquote the quotation and return
            UnquoteMacro(quotation)
      
      // use the quoation macro to parse the value into a class expression
      // use that with (v) => (v) -> (class-value) to create a quoation
      // incorporate that into a new quotation, use the generated quotation's lifts and runtime lifts
      // the output value
      // use the Unquote macro to take it back to an 'Insert[T]'

      unquotation
    }
      
  end Pipeline
  
  def apply[T: Type, Parser <: ParserFactory: Type](entityRaw: Expr[EntityQuery[T]], bodyRaw: Expr[T])(using Quotes): Expr[Insert[T]] =
    new Pipeline(entityRaw, bodyRaw).apply

}
