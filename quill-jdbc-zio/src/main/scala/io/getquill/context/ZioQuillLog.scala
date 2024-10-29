package io.getquill.context

import zio.FiberRef

object ZioQuillLog {

  val currentExecutionInfo: FiberRef[Option[ExecutionInfo]] =
    FiberRef.unsafe.make[Option[ExecutionInfo]](None)(Unsafe.unsafe)

  final class ExecutionInfoInformed(val executionInfo: () => ExecutionInfo) { self =>
    def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      currentExecutionInfo.locallyWith(_ => Some(executionInfo()))(zio)
  }

  def withExecutionInfo(info: => ExecutionInfo): ExecutionInfoInformed =
    new ExecutionInfoInformed(() => info)
}
