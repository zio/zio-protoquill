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
import java.time.LocalDateTime

/**
 * Trait that allows usage of 'static' block. Can declared one of these and use similar to encoders
 * but it needs to be compiled in a previous compilation unit and a global static.
 * TODO More explanation
 */
trait ToSql[T]:
  def toSql(value: T): String

trait ToString[T]:
  def toString(value: T): String

trait FromString[T]:
  def fromString(value: String): T

// Most to-sql splices are just the string-value e.g. for numbers. For strings, dates,
// etc... need to surround with quotes
// and various other complex things.
trait StringCodec[T] extends ToString[T] with FromString[T] with ToSql[T]:
  def toSql(value: T): String = toString(value)

import io.getquill.metaprog.Extractors._

object StringCodec:
  object ToSql:
    def summon[T: Type](using Quotes): Either[String, ToSql[T]] =
      import quotes.reflect.{Try => TTry, _}
      for {
        summonValue <- Expr.summon[io.getquill.ToSql[T]].toEitherOr(s"a ToString[${Format.TypeOf[T]}] cannot be summoned")
        // Summoning ToSql[T] will given (SpliceString: ToSql[String])
        // (a.k.a. Typed(Ident(SpliceString), TypeTree(ToSql[String])) usually with an outer inline surrounding it all)
        // so then we need to use Untype to just get SpliceString which is a module that we can load
        staticSpliceType = Untype(summonValue.asTerm.underlyingArgument).tpe.widen
        untypedModule <- Load.Module.fromTypeRepr(staticSpliceType).toEither.mapLeft(_.getMessage)
        module <- Try(untypedModule.asInstanceOf[io.getquill.ToSql[T]]).toEither.mapLeft(_.getMessage)
      } yield (module)
  object FromString:
    def summonExpr[T: Type](using Quotes) =
      val output = Expr.summon[io.getquill.FromString[T]].toEitherOr(s"a FromString[${Format.TypeOf[T]}] cannot be summoned")
      println(s"============== SUMMON RESULT ${Format.TypeOf[T]}: ${output}")
      output
end StringCodec

// Special case for splicing a string directly i.e. need to add 'single-quotes'
object SpliceString extends StringCodec[String]:
  override def toSql(value: String): String = s"'${value}'"
  def toString(value: String) = value
  def fromString(value: String) = value
implicit inline def stringCodec: StringCodec[String] = SpliceString

object SpliceInt extends StringCodec[Int]:
  def toString(value: Int) = s"${value}"
  def fromString(value: String) = value.toInt
implicit inline def intCodec: StringCodec[Int] = SpliceInt

object SpliceShort extends StringCodec[Short]:
  def toString(value: Short) = s"${value}"
  def fromString(value: String) = value.toShort
implicit inline def shortCodec: StringCodec[Short] = SpliceShort

object SpliceLong extends StringCodec[Long]:
  def toString(value: Long) = s"${value}"
  def fromString(value: String) = value.toLong
implicit inline def longCodec: ToString[Long] = SpliceLong

object SpliceFloat extends StringCodec[Float]:
  def toString(value: Float) = s"${value}"
  def fromString(value: String) = value.toFloat
implicit inline def floatCodec: StringCodec[Float] = SpliceFloat

object SpliceDouble extends StringCodec[Double]:
  def toString(value: Double) = s"${value}"
  def fromString(value: String) = value.toDouble
implicit inline def doubleCodec: StringCodec[Double] = SpliceDouble

private[getquill] object DateFormats:
  val parseFormat = DateTimeFormatter.ofPattern("[yyyyMMdd][yyyy-MM-dd]")
  val printFormat = DateTimeFormatter.ofPattern("yyyyMMdd")

object SpliceDate extends StringCodec[java.sql.Date]:
  override def toSql(value: java.sql.Date): String = s"'${toString(value)}'"
  def toString(value: java.sql.Date) = value.toLocalDate.format(DateFormats.printFormat)
  def fromString(value: String) =
    val local = LocalDate.parse(value, DateFormats.parseFormat)
    java.sql.Date.valueOf(local)
implicit inline def dateCodec: StringCodec[java.sql.Date] = SpliceDate

object SpliceLocalDate extends StringCodec[java.time.LocalDate]:
  override def toSql(value: java.time.LocalDate): String = s"'${toString(value)}'"
  def toString(value: java.time.LocalDate) = value.format(DateFormats.printFormat)
  def fromString(value: String) = LocalDate.parse(value, DateFormats.parseFormat)
implicit inline def localDateCodec: StringCodec[java.time.LocalDate] = SpliceLocalDate

object SpliceLocalDateTime extends StringCodec[java.time.LocalDateTime]:
  override def toSql(value: java.time.LocalDateTime): String = s"'${toString(value)}'"
  def toString(value: java.time.LocalDateTime) = value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  def fromString(value: String) = LocalDateTime.parse(value, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
implicit inline def localDateTimeCodec: StringCodec[java.time.LocalDateTime] = SpliceLocalDateTime
