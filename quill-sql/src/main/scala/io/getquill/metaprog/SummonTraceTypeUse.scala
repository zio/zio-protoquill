package io.getquill.metaprog

import io.getquill.norm.EnableTrace
import io.getquill.util.Messages.TraceType
import io.getquill.norm.DisablePhase
import io.getquill.norm.OptionalPhase

// TODO Move this to a test
object SummonTraceTypeUse {
  def main(args: Array[String]): Unit = {
    import io.getquill.norm.ConfigList._
    given EnableTrace with
      override type Trace = TraceType.ApplyMap :: HNil

    given DisablePhase with
      override type Phase = OptionalPhase.ApplyMap :: HNil

    SummonTranspileConfig.mac //
  }
}
