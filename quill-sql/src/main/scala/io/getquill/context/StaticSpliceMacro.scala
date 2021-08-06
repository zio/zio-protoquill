package io.getquill.context

import scala.quoted._
import io.getquill.StaticSplice
import io.getquill.util.LoadModule
import io.getquill.metaprog.Extractors
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.util.Right
import scala.util.Left
import io.getquill.util.Format
import io.getquill.Quoted
import io.getquill.quat.QuatMaking
import io.getquill.parser.Lifter
import scala.util.Try

object StaticSpliceMacro {
  import Extractors._

  private[getquill] object SelectPath:
    def recurseInto(using Quotes)(term: quotes.reflect.Term, accum: List[String] = List()): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        // Select(Select(Ident("core"), "foo"), "bar") => recurseInto( {Select(Ident("core"), "foo")}, "bar" +: List("baz") )
        case Select(inner, pathNode) => recurseInto(inner, pathNode +: accum)
        case id: Ident => Some((id, accum))
        // If at the core of the nested selects is not a Ident, this does not match
        case other => None

    def unapply(using Quotes)(term: quotes.reflect.Term): Option[(quotes.reflect.Term, List[String])] =
      import quotes.reflect._
      term match
        case select: Select => recurseInto(select)
        case id: Ident => Some((id, List()))
        case _ => None
  end SelectPath

  extension [T](opt: Option[T])
    def nullAsNone =
      opt match
        case Some(null) => None
        case _ => opt

  object DefTerm:
    def unapply(using Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Term] =
      import quotes.reflect._
      if (term.tpe.termSymbol.isValDef || term.tpe.termSymbol.isDefDef) Some(term)
      else None

  // object StaticLineage {
  //   case class Module(using Quotes)(tpe: quotes.reflect.TypeRepr) extends StaticLineage
  //   case class Package(using Quotes)(term: Term) extends StaticLineage

  // }

  object TermOwnerModule:
    def unapply(using Quotes)(value: quotes.reflect.Term): Option[quotes.reflect.TypeRepr] =
      import quotes.reflect.{Try => _, _}
      Try(value.tpe.termSymbol.owner).toOption.flatMap { owner =>
        val memberType = value.tpe.memberType(owner)
        if (memberType.typeSymbol.flags.is(Flags.Module))
          Some(memberType)
        else
          None
      }



  def apply[T: Type](value: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.{ Try => _, _ }
    import ReflectivePathChainLookup.StringOps._

    val owner = value.asTerm.tpe.memberType(value.asTerm.tpe.termSymbol.owner)
    println(
      s"INPUT: ${Printer.TreeStructure.show(value.asTerm)}\n" +
      s"IS VAL: ${value.asTerm.tpe.termSymbol.isValDef}\n" +
      s"SYM: ${owner.typeSymbol.flags.show}"
    )

    value.asTerm match
      case term @ DefTerm(TermOwnerModule(owner)) =>
        println(s"Owner of: ${term} is ${owner}")

      case _ =>

    // TODO summon a Expr[StaticSplicer] using the T type passed originally.
    // Then use use LoadModule to get the value of that thing during runtime so we can use it
    // (i.e. see io.getquill.metaprog.SummonParser on how to do that)
    // for primitive types e.g. String, Int, Float etc... rather then making summonable splicers
    // it is easier to just splice them directly, since otherwise those StaticSplicer modules themselves
    // need to be compiled in a previous compilation unit, and we want to define them here.
    // Technically that should be fine because they will only actually be used in the testing code though
    // should think about this more later. For now just do toString to check that stuff from the main return works


    Untype(value.asTerm.underlyingArgument) match
      case SelectPath(pathRoot, selectedPath) =>

        // selectedOwner can just be the method name so we need to find it's owner and all that to the path
        // (e.g. for `object Foo { def fooMethod }` it could just be Ident(fooMethod))
        val (ownerTpe, path) =
          pathRoot match
            case term @ DefTerm(TermOwnerModule(owner)) =>
              // Add name of the method to path of things we are selected, owner is now the owner of that method
              // (I.e. `Foo` is the owner module, `fooMethod` is the name added to the path)
              println(s"************ NEXT OWNER IS: ${owner.typeSymbol.owner} ************")
              (owner, selectedPath :+ pathRoot.symbol.name)
            case _ =>
              (pathRoot.tpe, selectedPath)

        // TODO Check if ident is a static module, throw an error otherwise
        val module = LoadModule.TypeRepr(ownerTpe)

        println("============= TRYING TO LOAD STUFF =========")
        println( Try(Class.forName("io.getquill.util.prep.Mod$.Foo$.Bar$")) )
        println( Try(Class.forName("io.getquill.util.prep.Mod$Foo$Bar$")) )
        println( Try(Class.forName("io.getquill.util.prep.Mod$Foo$Bar$")).map(_.getMethods.toList.map(_.getName)) )

        val splicedValue =
          module match
            case Success(value) =>
              ReflectivePathChainLookup(value, path) match
                case Right(value) => value
                case Left(msg) =>
                  report.throwError(s"Could not look up {${(ownerTpe)}}.${path.mkString(".")}. Failed because:\n${msg}")
            case Failure(e) =>
              // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
              report.throwError(s"Could not look up {${(ownerTpe)}}.${path.mkString(".")} from the object.\nStatic load failed due to: ${e.getMessage}")

        // TODO Summon StaticSplicer here
        // TODO Maybe 'ast.Constant' should be reserved for actual scala constants and this should be a new type ast.Literal of some kind?
        import io.getquill.ast._
        val quat = Lifter.quat(QuatMaking.ofType[T])

        val splice = Expr(s""""${splicedValue.current}"""")

        val quotation =
          UnquoteMacro('{ Quoted[T](Infix(List($splice), List(), true, $quat),Nil, Nil) })

        println("============ Output Quotation ============\n" + Format.Expr(quotation))
        quotation

      case other =>
        // TODO Long explanatory message about how it has to some value inside object foo inside object bar... and it needs to be a thing compiled in a previous compilation unit
        report.throwError(s"Could not load a static value `${Format.Expr(value)}` from ${Printer.TreeStructure.show(other)}")
}