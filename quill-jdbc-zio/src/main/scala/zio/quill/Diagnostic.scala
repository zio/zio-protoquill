package zio.quill

import zio.ZIO
import zio.UIO
import zio.quill.context.{ExecutionInfo, ZioQuillLog}

def getLastExecutedQuery(): UIO[Option[String]] =
  ZioQuillLog.latestSqlQuery.get

def getLastExecutionInfo(): UIO[Option[ExecutionInfo]] =
  ZioQuillLog.latestExecutionInfo.get