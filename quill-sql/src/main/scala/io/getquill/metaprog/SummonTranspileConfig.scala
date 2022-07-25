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

object SummonTranspileConfig:
  // TODO Move the actual macro that calls this to a test. The regular code should only use SummonTranspileConfig.apply inside of other macros
  inline def mac: Unit = ${ macImpl }
  def macImpl(using Quotes): Expr[TranspileConfig] =
    val config = apply()
    TranspileConfigLiftable(config)

  def apply()(using Quotes): TranspileConfig =
    import quotes.reflect._
    val traceTypes = summonTraceTypes()
    val disabledPhases = summonPhaseDisables()
    val conf = TranspileConfig(disabledPhases, TraceConfig(traceTypes))
    // report.info(conf.toString)
    conf

  def summonTraceTypes()(using Quotes): List[TraceType] =
    import quotes.reflect._
    val enableTraceExpr = Expr.summon[EnableTrace].getOrElse('{ EnableTraceNone })
    val foundTraceTypeNames = findHListMembers(enableTraceExpr, "Trace").map(_.typeSymbol.name)
    TraceType.values.filter { trace =>
      val simpleName = parseSealedTraitClassName(trace.getClass)
      foundTraceTypeNames.contains(simpleName)
    }

  def summonPhaseDisables()(using Quotes): List[OptionalPhase] =
    import quotes.reflect._
    val disablePhaseExpr = Expr.summon[DisablePhase].getOrElse('{ DisablePhaseNone })
    val disablePhaseTypeNames = findHListMembers(disablePhaseExpr, "Phase").map(_.typeSymbol.name)
    OptionalPhase.all.filter { phase =>
      val simpleName = parseSealedTraitClassName(phase.getClass)
      disablePhaseTypeNames.contains(simpleName)
    }

  def findHListMembers(baseExpr: Expr[_], typeMemberName: String)(using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect._
    val memberSymbol = baseExpr.asTerm.tpe.termSymbol.memberType(typeMemberName)
    val hlistType = baseExpr.asTerm.select(memberSymbol).tpe.widen
    val extractedTypes = recurseConfigList(hlistType.asType)
    extractedTypes.map { case '[t] => TypeRepr.of[t] }.toList

  private def parseSealedTraitClassName(cls: Class[_]) =
    cls.getName.stripSuffix("$").replaceFirst("(.*)[\\.$]", "")

  private def recurseConfigList(listMember: Type[_])(using Quotes): List[Type[_]] =
    import quotes.reflect._
    listMember match
      case '[HNil] => Nil
      case '[head :: tail] =>
        Type.of[head] :: recurseConfigList(Type.of[tail])
      case _ =>
        report.throwError(s"Invalid config list member type: ${Format.Type(listMember)}. Need to be either :: or HNil types.")

end SummonTranspileConfig

private[getquill] object TranspileConfigLiftable:
  def apply(transpileConfig: TranspileConfig)(using Quotes) =
    liftableTranspileConfig(transpileConfig)

  extension [T](t: T)(using ToExpr[T], Quotes)
    def expr: Expr[T] = Expr(t)

  import io.getquill.parser.BasicLiftable
  import io.getquill.util.Messages.TraceType

  given liftableOptionalPhase: BasicLiftable[OptionalPhase] with
    def lift =
      case OptionalPhase.ApplyMap => '{ OptionalPhase.ApplyMap }

  given liftableTraceType: BasicLiftable[TraceType] with
    def lift =
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

  given liftableTraceConfig: BasicLiftable[TraceConfig] with
    def lift =
      case TraceConfig(enabledTraces) => '{ io.getquill.util.TraceConfig(${ enabledTraces.expr }) }

  given liftableTranspileConfig: BasicLiftable[TranspileConfig] with
    def lift =
      case TranspileConfig(disablePhases, traceConfig) => '{ io.getquill.norm.TranspileConfig(${ disablePhases.expr }, ${ traceConfig.expr }) }

end TranspileConfigLiftable
