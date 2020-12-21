
package io.getquill.context

import io.getquill._
import scala.quoted._
import miniquill.quoter.UnquoteMacro
import io.getquill.norm.BetaReduction
import io.getquill.util.LoadObject 
import miniquill.parser.Parser
import miniquill.parser.Parser.Implicits._
import miniquill.parser.ParserFactory
import io.getquill.derived.Expander
import io.getquill.ast.{ Ident => AIdent, Insert => AInsert, _ }
import miniquill.quoter.LiftExtractHelper
import miniquill.quoter.Quoted
import miniquill.parser.Lifter
import miniquill.quoter.UnquoteMacro

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

  
  def apply[T: Type, Parser <: ParserFactory: Type](bodyRaw: Expr[T])(using Quotes): Expr[Insert[T]] = {
    import quotes.reflect._
    val body = bodyRaw.asTerm.underlyingArgument.asExpr

    val parserFactory = LoadObject[Parser].get
    import Parser._

    val rawAst = parserFactory.apply.seal.apply(body)
    // This shuold be a CaseClass (TODO Should probably verify that)
    val insertCaseClassAst = BetaReduction(rawAst)

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

    val pluckedUnquotes = LiftExtractHelper.extractRuntimeUnquotes(bodyRaw)
    val lifts = LiftExtractHelper.extractLifts(bodyRaw)

    // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)

    // TODO if there is a schemaMeta need to use that to create the entity
    val entityName = TypeRepr.of[T].classSymbol.get.name
    val entity = Entity(entityName, List())
    val insert = AInsert(entity, assignmentsAst)


    val reifiedInsert = Lifter(insert)
    val quotation = 
      '{ Quoted[Insert[T]](${reifiedInsert}, ${Expr.ofList(lifts)}, ${Expr.ofList(pluckedUnquotes)}) }

    val unquotation = UnquoteMacro(quotation)
    
    
    // use the quoation macro to parse the value into a class expression
    // use that with (v) => (v) -> (class-value) to create a quoation
    // incorporate that into a new quotation, use the generated quotation's lifts and runtime lifts
    // the output value
    // use the Unquote macro to take it back to an 'Insert[T]'

    unquotation
  }

}
