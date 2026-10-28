package zio.quill.context.qzio

import zio.quill.NamingStrategy
import zio.quill.context.{ Context, ContextEffect }
import zio.quill.idiom.Idiom
import zio.ZIO
import zio.quill.context.ContextTranslateMacro

trait ZioTranslateContext[+Dialect <: zio.quill.idiom.Idiom, +Naming <: NamingStrategy]
  extends Context[Dialect, Naming]
  with ContextTranslateMacro[Dialect, Naming] {

  type Error
  type Environment

  override type TranslateResult[T] = ZIO[Environment, Error, T]
  override def wrap[T](t: => T): TranslateResult[T] = ZIO.environment[Environment].as(t)
  override def push[A, B](result: TranslateResult[A])(f: A => B): TranslateResult[B] = result.map(f)
  override def seq[A](list: List[TranslateResult[A]]): TranslateResult[List[A]] = ZIO.collectAll(list)
}
