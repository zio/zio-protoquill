package io.getquill

import scala.quoted._
import java.time.format.DateTimeFormatter
import io.getquill.util.Format
import io.getquill.util.Load
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import io.getquill.util.CommonExtensions.Either._
import java.time.LocalDate

/**
 * Trait that allows usage of 'static' block. Can declared one of these and use similar to encoders
 * but it needs to be compiled in a previous compilation unit and a global static.
 * TODO More explanation
 */
trait ToString[T]:
  def toString(value: T): String

trait FromString[T]:
  def fromString(value: String): T

trait StringCodec[T] extends ToString[T] with FromString[T]

import io.getquill.metaprog.Extractors._

object StringCodec:
  object ToString:
    def summon[T: Type](using Quotes): Either[String, ToString[T]] =
      import quotes.reflect.{Try => TTry, _}
      for {
        summonValue <- Expr.summon[io.getquill.ToString[T]].toEitherOr(s"a ToString[${Format.TypeOf[T]}] cannot be summoned")
        // Summoning ToString[T] will given (SpliceString: ToString[String])
        // (a.k.a. Typed(Ident(SpliceString), TypeTree(ToString[String])) usually with an outer inline surrounding it all)
        // so then we need to use Untype to just get SpliceString which is a module that we can load
        staticSpliceType = Untype(summonValue.asTerm.underlyingArgument).tpe.widen
        untypedModule <- Load.Module.fromTypeRepr(staticSpliceType).toEither.mapLeft(_.getMessage)
        module <- Try(untypedModule.asInstanceOf[io.getquill.ToString[T]]).toEither.mapLeft(_.getMessage)
      } yield (module)
  object FromString:
    def summonExpr[T: Type](using Quotes) =
      Expr.summon[io.getquill.FromString[T]].toEitherOr(s"a FromString[${Format.TypeOf[T]}] cannot be summoned")
end StringCodec

// Special case for splicing a string directly i.e. need to add 'single-quotes'
object SpliceString extends ToString[String]:
  def toString(value: String) = s"'${value}'"
inline given ToString[String] = SpliceString

object SpliceInt extends StringCodec[Int]:
  def toString(value: Int) = s"${value}"
  def fromString(value: String) = value.toInt
inline given StringCodec[Int] = SpliceInt

object SpliceShort extends StringCodec[Short]:
  def toString(value: Short) = s"${value}"
  def fromString(value: String) = value.toShort
inline given StringCodec[Short] = SpliceShort

object SpliceLong extends StringCodec[Long]:
  def toString(value: Long) = s"${value}"
  def fromString(value: String) = value.toLong
inline given ToString[Long] = SpliceLong

object SpliceFloat extends StringCodec[Float]:
  def toString(value: Float) = s"${value}"
  def fromString(value: String) = value.toFloat
inline given StringCodec[Float] = SpliceFloat

object SpliceDouble extends StringCodec[Double]:
  def toString(value: Double) = s"${value}"
  def fromString(value: String) = value.toDouble
inline given StringCodec[Double] = SpliceDouble

object SpliceDate extends StringCodec[java.sql.Date]:
  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMdd")
  def toString(value: java.sql.Date) = value.toLocalDate.format(dateFormat)
  def fromString(value: String) =
    val local = LocalDate.parse(value, dateFormat)
    java.sql.Date.valueOf(local)
inline given StringCodec[java.sql.Date] = SpliceDate

object SpliceLocalDate extends StringCodec[java.time.LocalDate]:
  private val dateFormat = DateTimeFormatter.ofPattern("yyyyMMdd")
  def toString(value: java.time.LocalDate) = value.format(dateFormat)
  def fromString(value: String) = LocalDate.parse(value, dateFormat)
inline given StringCodec[java.time.LocalDate] = SpliceLocalDate
