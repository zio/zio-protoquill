
package io.getquill.context

import io.getquill._
import scala.quoted._
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import io.getquill.parser.Parser
import io.getquill.parser.Parser.Implicits._
import io.getquill.parser.ParserFactory
import io.getquill.derived.Expander
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, _ }
import io.getquill.parser.Lifter
import io.getquill.parser.Unlifter
import io.getquill.quoter.QuotationLotExpr
import io.getquill.quoter.QuotationLotExpr._
import io.getquill.parser.TastyMatchers
import io.getquill.quoter.Quoted

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
 * {code} Assignment(Id(v), Prop(Id(v), name), Const("Joe")) {code}
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
 * The end result of this synthesis is a series of assignments for an insert for the given entity
 */
object InsertMacro {

  class Pipeline[T: Type, Parser <: ParserFactory: Type](entityRaw: Expr[EntityQuery[T]], bodyRaw: Expr[T])(using val qctx: Quotes) extends TastyMatchers:
    import quotes.reflect._
    import io.getquill.util.Messages.qprint

    def plainEntity: Entity =
      val entityName = TypeRepr.of[T].classSymbol.get.name
      Entity(entityName, List())

    def processEntity: Entity =
      val underlyingEntity = entityRaw.asTerm.underlyingArgument.asExprOf[EntityQuery[T]]
      UntypeExpr(underlyingEntity) match 
        // If it is a plain entity query (as returned from QueryMacro)
        case '{ EntityQuery[t] } => plainEntity
        // If there are query schemas involved
        case QuotationLotExpr.Unquoted(unquotation) => unquotation match
          case Uprootable(_, ast, _, _, _, _) => 
            Unlifter(ast) match
              case ent: Entity => ent
              case other => report.throwError(s"Unlifted insertion Entity '${qprint(other).plainText}' is not a Query.")
          case _ =>
            println("WARNING: Only inline schema-metas are supported for insertions so far. Falling back to a plain entity.")
            plainEntity
        

    /** Parse the input to of query[Person].insert(Person("Joe", "Bloggs")) into CaseClass(firstName="Joe",lastName="Bloggs") */
    def parseAstCaseClass: CaseClass = {  
      val body = bodyRaw.asTerm.underlyingArgument.asExpr
      val parserFactory = LoadObject[Parser].get
      val rawAst = parserFactory.apply.seal.apply(body)
      val ast = BetaReduction(rawAst)
      // TODO Implement dynamic pipeline for schema meta
      ast match
        case cc: CaseClass => cc
        case _ => report.throwError(s"Parsed Insert Macro AST is not a Case Class: ${qprint(ast).plainText}")  
    }

    def synthesizeAssignments(insertCaseClassAst: CaseClass) = {
      // Expand into a AST
      // T:Person(name:Str, age:Option[Age]) Age(value: Int) -> Ast: List(v.name, v.age.map(v => v.value))
      val expansionList = Expander.staticList[T]("v")

      // Now synthesize (v) => vAssignmentProperty -> assignmentValue
      // e.g. (p:Person) => p.firstName -> "Joe"
      // TODO, Ast should always be a case class (maybe a tuple?) should verify that
      def mapping(path: Ast) =
        val reduction = BetaReduction(path, AIdent("v") -> insertCaseClassAst)
        Assignment(AIdent("v"), path, reduction)

      val assignmentsAst = expansionList.map(exp => mapping(exp))
      assignmentsAst
    }

    def synthesizeAssignmentsFromBody =
      val astCaseClass = parseAstCaseClass
      synthesizeAssignments(astCaseClass)


    def apply = {
      val assignmentsAst = synthesizeAssignmentsFromBody

      // Insertion could have lifts and quotes, need to extract those
      val (lifts, pluckedUnquotes) = ExtractLifts(bodyRaw)    

      // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)
      // TODO if there is a schemaMeta need to use that to create the entity
      val entity = processEntity
      val assignments = assignmentsAst.map(asi => Lifter.assignment(asi))
      val insert = '{ AInsert(${Lifter.entity(entity)}, ${Expr.ofList(assignments)}) }

      val quotation = 
        '{ Quoted[Insert[T]](${insert}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }

      val unquotation = UnquoteMacro(quotation)
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
