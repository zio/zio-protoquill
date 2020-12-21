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

    // Now syntheize the function "(v) => vAssignmentProperty -> assignmentValue" (on the AST)
    // Given 
    //  case class Person(name: String, age: Option[Age]); Age(value: Int)
    //  the User has done query[Person].insert(Person("Joe", Age(345)))
    //
    // e.g.   (v: Person) => v.name -> (v:Person).name
    // a.k.a  Assignment(Id(v), Prop(Id(v), name), Const("Joe"))
    // and    (v: Person) => v.age.map(v => v.value) -> Option(v:Age).map(v => v.value)
    // a.k.a  Assignment(Id(v), 
    //                   OptionTableMap(Prop(Id(v), age), Id(v), Prop(Id(v), value))
    //                   OptionTableMap(OptionApply(CaseClass(value=345)), Id(v), Prop(Id(v), value))
    //        )
    // Note that these are represented as values on the AST as opposed to actual scala code
    //   where (v) => foo -> bar is Assignment(v, foo, bar) // where foo = path to property, bar = path to property with case-class subtitution
    //
    // The end result of this synthesis is a series of assignments for an insert for the given entity

    // TODO, Ast should always be a case class (maybe a tuple?) should verify that
    def mapping(path: Ast) =
      val reduction = BetaReduction(path, AIdent("v") -> insertCaseClassAst)
      Assignment(AIdent("v"), path, reduction)

    val insertions = expansionList.map(exp => mapping(exp))
    println("************ Insertions **********")
    println(io.getquill.util.Messages.qprint(insertions))


    //println("Ast Is: " + ast)

    // TODO If implicit insert meta, need to detect it and use it instead (i.e. with exclusions)
    
    // use the quoation macro to parse the value into a class expression
    // use that with (v) => (v) -> (class-value) to create a quoation
    // incorporate that into a new quotation, use the generated quotation's lifts and runtime lifts
    // the output value
    // use the Unquote macro to take it back to an 'Insert[T]'


    '{ ??? }
  }

}