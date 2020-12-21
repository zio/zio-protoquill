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
 * This macro transforms a insert query in the form query[Person].insert(Person("Joe", "Bloggs"))
 * into the form query[Person].insert(_.firstName -> "Joe", _.lastName -> "Bloggs").
 * There are various other important details such as if a field is optional e.g. 
 * {code}
 *   case class Person(name: String, age: Option[Age]); Age(value: Int)
 * {code}
 * The assignments are a bit more complex e.g.:
 * {code}
 *   query[Person].insert("Joe", Some(Age(123))) // becomes
 *     query[Person].insert(_.name -> "Joe", _.age.map(v => v.value) -> Some(Age(123)).map(v => v.value))
 * {code}
 * Note that we don't actually create the code for assignments but instead generate the corresponding
 * AST that it will have once it goes through the parser. This avoids the inefficiency of having
 * to generate code that we will then later parse.
 * The Expander actually does most of the effort of this conversion so most of the heavy lifting is already done.
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
    println("************ Parsed The Ast **********")
    println(io.getquill.util.Messages.qprint(insertCaseClassAst))

    // Expand into a AST
    // T:Person(name:Str, age:Option[Age]) Age(value: Int) -> Ast: List(v.name, v.age.map(v => v.value))
    println("************ Expanded The Ast **********")
    val expansionList = Expander.staticList[T]("v")
    println(io.getquill.util.Messages.qprint(expansionList))

    /*
     * Now syntheize the function "(v) => vAssignmentProperty -> assignmentValue" (on the AST)
     * Given 
     *  case class Person(name: String, age: Option[Age]); Age(value: Int)
     *  the User has done query[Person].insert(Person("Joe", Age(345)))
     *
     * e.g.   (v: Person) => v.name -> (v:Person).name
     * a.k.a  Assignment(Id(v), Prop(Id(v), name), Const("Joe"))
     * and    (v: Person) => v.age.map(v => v.value) -> Option(v:Age).map(v => v.value)
     * a.k.a  Assignment(Id(v), 
     *                   OptionTableMap(Prop(Id(v), age), Id(v), Prop(Id(v), value))
     *                   OptionTableMap(OptionApply(CaseClass(value=345)), Id(v), Prop(Id(v), value))
     *        )
     * Note that these are represented as values on the AST as opposed to actual scala code
     *   where (v) => foo -> bar is Assignment(v, foo, bar) // where foo = path to property, bar = path to property with case-class subtitution
     * 
     * The end result of this synthesis is a series of assignments for an insert for the given entity
     */
    
    // TODO, Ast should always be a case class (maybe a tuple?) should verify that
    def mapping(path: Ast) =
      val reduction = BetaReduction(path, AIdent("v") -> insertCaseClassAst)
      Assignment(AIdent("v"), path, reduction)

    val assignmentsAst = expansionList.map(exp => mapping(exp))
    println("************ Insertions **********")
    println(io.getquill.util.Messages.qprint(assignmentsAst))

    val pluckedUnquotes = LiftExtractHelper.extractRuntimeUnquotes(bodyRaw)
    val lifts = LiftExtractHelper.extractLifts(bodyRaw)

    // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)

    // TODO if there is a schemaMeta need to use that to create the entity
    val entityName = TypeRepr.of[T].classSymbol.get.name
    val entity = Entity(entityName, List())

    val insert = AInsert(entity, assignmentsAst)
   
    println("************ OUTPUT **********")
    println(io.getquill.util.Messages.qprint(insert))

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