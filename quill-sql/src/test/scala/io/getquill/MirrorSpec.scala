package io.getquill

import io.getquill.context.mirror.{MirrorSession, Row}

trait MirrorSpec {
  type SpecSession = MirrorSession
  type SpecPrepareRow = Row
  type SpecResultRow = Row
}
