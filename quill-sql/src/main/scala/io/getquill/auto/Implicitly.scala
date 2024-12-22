package io.getquill.auto

trait AutoDerive[T]

object AutoDerive {
  object Implicitly {
    implicit def autoDerive[T]: AutoDerive[T] = new AutoDerive[T] {}
  }
}
