package io.getquill

import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import io.getquill.Quoted
import io.getquill.EagerPlanter
import io.getquill.ast.*
import io.getquill.quat.Quat
import io.getquill.NamingStrategy
import io.getquill.idiom.Idiom
import io.getquill.Query
import io.getquill.context.mirror.Row
import io.getquill.generic.StandardCodec

/** A general spec for tests that don't even necessarily import a context */
trait ProtoSpec extends  AnyFreeSpec with Matchers with BeforeAndAfterAll

trait Spec extends ProtoSpec with StandardCodec with TestEntities {
  val QV = Quat.Value
  def QEP(name: String) = Quat.Product.empty(name)
  def QP(name: String, fields: String*) = Quat.LeafProduct(name, fields: _*)

  def await[T](f: Future[T]): T = Await.result(f, Duration.Inf)

  extension (quat: Quat) {
    def productOrFail() =
      quat match {
        case p: Quat.Product => p
        case _               => throw new IllegalArgumentException(s"The quat ${quat} is expected to be a product but is not")
      }
  }

  case class NameChangeIdent(nameChange: PartialFunction[String, String]) extends StatelessTransformer {
    override def applyIdent(id: Ident) = id.copy(name = nameChange.lift(id.name).getOrElse(id.name))
  }
}
