package io.getquill

import zio.ZIO
import zio.UIO
import io.getquill.context.{ExecutionInfo, ZioQuillLog}

def getLastExecutedQuery(): UIO[Option[String]] =
  ZioQuillLog.latestSqlQuery.get

def getLastExecutionInfo(): UIO[Option[ExecutionInfo]] =
  ZioQuillLog.latestExecutionInfo.get