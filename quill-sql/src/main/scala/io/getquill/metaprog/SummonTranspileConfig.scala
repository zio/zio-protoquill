package io.getquill.metaprog

import scala.quoted._
import io.getquill.norm.TranspileConfig
import io.getquill.norm.OptionalPhase
import io.getquill.util.TraceConfig
import io.getquill.norm.EnableTrace
import io.getquill.norm.EnableTraceNone
import io.getquill.norm.ConfigList._
import io.getquill.util.Format
import io.getquill.util.Messages.TraceType
import io.getquill.norm.DisablePhase
import io.getquill.norm.DisablePhaseNone
import io.getquill.parser.Lifters

object SummonTranspileConfig {
  // TODO Move the actual macro that calls this to a test. The regular code should only use SummonTranspileConfig.apply inside of other macros
  inline def mac: TranspileConfig = ${ macImpl }
  def macImpl(using Quotes): Expr[TranspileConfig] = {
    val config = apply()
    TranspileConfigLiftable(config)
  }

  def apply()(using Quotes): TranspileConfig = {
    import quotes.reflect._
    val traceTypes = summonTraceTypes()
    val disabledPhases = summonPhaseDisables()
    val conf = TranspileConfig(disabledPhases, TraceConfig(traceTypes))
    // report.info(conf.toString)
    conf
  }

  def summonTraceTypes(orFromProperties: Boolean = false)(using Quotes): List[TraceType] = {
    import quotes.reflect._
    Expr.summon[EnableTrace] match {
      case Some(enableTraceExpr) =>
        val foundTraceTypeNames = findHListMembers(enableTraceExpr, "Trace").map(_.typeSymbol.name)
        TraceType.values.filter { trace =>
          val simpleName = parseSealedTraitClassName(trace.getClass)
          foundTraceTypeNames.contains(simpleName)
        }
      case None =>
        if (orFromProperties)
          io.getquill.util.GetTraces()
        else
          List()
    }
  }

  def summonPhaseDisables()(using Quotes): List[OptionalPhase] = {
    import quotes.reflect._
    Expr.summon[DisablePhase] match {
      case Some(disablePhaseExpr) =>
        val disablePhaseTypeNames = findHListMembers(disablePhaseExpr, "Phase").map(_.typeSymbol.name)
        OptionalPhase.all.filter { phase =>
          val simpleName = parseSealedTraitClassName(phase.getClass)
          disablePhaseTypeNames.contains(simpleName)
        }
      case None => List()
    }
  }

  def findHListMembers(baseExpr: Expr[_], typeMemberName: String)(using Quotes): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect._
    val memberSymbol = baseExpr.asTerm.tpe.termSymbol.memberType(typeMemberName)
    val hlistType = baseExpr.asTerm.select(memberSymbol).tpe.widen
    val extractedTypes = recurseConfigList(hlistType.asType)
    extractedTypes.map { case '[t] => TypeRepr.of[t] }.toList
  }

  private def parseSealedTraitClassName(cls: Class[_]) =
    cls.getName.stripSuffix("$").replaceFirst("(.*)[\\.$]", "")

  private def recurseConfigList(listMember: Type[_])(using Quotes): List[Type[_]] = {
    import quotes.reflect._
    listMember match {
      case '[HNil] => Nil
      case '[head :: tail] =>
        Type.of[head] :: recurseConfigList(Type.of[tail])
      case _ =>
        report.throwError(s"Invalid config list member type: ${Format.Type(listMember)}. Need to be either :: or HNil types.")
    }
  }

} // end SummonTranspileConfig

private[getquill] object TranspileConfigLiftable {
  def apply(transpileConfig: TranspileConfig)(using Quotes): Expr[TranspileConfig] =
    liftTranspileConfig(transpileConfig)
  def apply(traceConfig: TraceConfig)(using Quotes): Expr[TraceConfig] =
    liftTraceConfig(traceConfig)

  extension [T](t: T)(using ToExpr[T], Quotes) {
    def expr: Expr[T] = Expr(t)
  }

  import io.getquill.parser.Lifters.Plain
  import io.getquill.util.Messages.TraceType

  given liftOptionalPhase: Lifters.Plain[OptionalPhase] with {
    def lift = {
      case OptionalPhase.ApplyMap => '{ OptionalPhase.ApplyMap }
    }
  }

  given liftTraceType: Lifters.Plain[TraceType] with {
    def lift = {
      case TraceType.SqlNormalizations      => '{ TraceType.SqlNormalizations }
      case TraceType.ExpandDistinct         => '{ TraceType.ExpandDistinct }
      case TraceType.Normalizations         => '{ TraceType.Normalizations }
      case TraceType.Standard               => '{ TraceType.Standard }
      case TraceType.NestedQueryExpansion   => '{ TraceType.NestedQueryExpansion }
      case TraceType.AvoidAliasConflict     => '{ TraceType.AvoidAliasConflict }
      case TraceType.ShealthLeaf            => '{ TraceType.ShealthLeaf }
      case TraceType.ReifyLiftings          => '{ TraceType.ReifyLiftings }
      case TraceType.PatMatch               => '{ TraceType.PatMatch }
      case TraceType.Quotation              => '{ TraceType.Quotation }
      case TraceType.RepropagateQuats       => '{ TraceType.RepropagateQuats }
      case TraceType.RenameProperties       => '{ TraceType.RenameProperties }
      case TraceType.ApplyMap               => '{ TraceType.ApplyMap }
      case TraceType.Warning                => '{ TraceType.Warning }
      case TraceType.ExprModel              => '{ TraceType.ExprModel }
      case TraceType.Meta                   => '{ TraceType.Meta }
      case TraceType.Execution              => '{ TraceType.Execution }
      case TraceType.DynamicExecution       => '{ TraceType.DynamicExecution }
      case TraceType.Elaboration            => '{ TraceType.Elaboration }
      case TraceType.SqlQueryConstruct      => '{ TraceType.SqlQueryConstruct }
      case TraceType.FlattenOptionOperation => '{ TraceType.FlattenOptionOperation }
      case TraceType.Particularization      => '{ TraceType.Particularization }
    }
  }

  given liftTraceConfig: Lifters.Plain[TraceConfig] with {
    def lift = {
      case TraceConfig(enabledTraces) => '{ io.getquill.util.TraceConfig(${ enabledTraces.expr }) }
    }
  }

  given liftTranspileConfig: Lifters.Plain[TranspileConfig] with {
    def lift = {
      case TranspileConfig(disablePhases, traceConfig) => '{ io.getquill.norm.TranspileConfig(${ disablePhases.expr }, ${ traceConfig.expr }) }
    }
  }

} // end TranspileConfigLiftable
