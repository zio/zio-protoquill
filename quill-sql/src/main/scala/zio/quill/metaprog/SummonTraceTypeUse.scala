package zio.quill.metaprog

import zio.quill.norm.EnableTrace
import zio.quill.util.Messages.TraceType
import zio.quill.norm.DisablePhase
import zio.quill.norm.OptionalPhase

// TODO Move this to a test
object SummonTraceTypeUse {
  def main(args: Array[String]): Unit = {
    import zio.quill.norm.ConfigList._
    given EnableTrace with {
      override type Trace = TraceType.ApplyMap :: HNil
    }

    given DisablePhase with {
      override type Phase = OptionalPhase.ApplyMap :: HNil
    }

    SummonTranspileConfig.mac //
  }
}
