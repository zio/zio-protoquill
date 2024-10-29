package io.getquill.context

import zio.*

object ZioQuillLog {
  val latestExecutionInfo: FiberRef[Option[ExecutionInfo]] =
    zio.Unsafe.unsafe {
      FiberRef.unsafe.make[Option[ExecutionInfo]](None)
    }

  final class ExecutionInfoAware(val executionInfo: () => ExecutionInfo) { self =>
    def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      latestExecutionInfo.set(Some(executionInfo())) *> zio
  }

  def withExecutionInfo(info: => ExecutionInfo): ExecutionInfoAware =
    new ExecutionInfoAware(() => info)


  val latestSqlQuery: FiberRef[Option[String]] =
    zio.Unsafe.unsafe {
      FiberRef.unsafe.make[Option[String]](None)
    }

  final class SqlQueryAware(val sqlQuery: () => String) {
    self =>
    def apply[R, E, A](zio: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
      latestSqlQuery.set(Some(sqlQuery())) *> zio
  }

  def withSqlQuery(sqlQuery: => String): SqlQueryAware =
    new SqlQueryAware(() => sqlQuery)
}

