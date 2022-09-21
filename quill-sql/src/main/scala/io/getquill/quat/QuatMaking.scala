package io.getquill.quat

import scala.quoted.{Type => TType, _}

// import io.getquill.Udt
import scala.annotation.tailrec
import scala.reflect.ClassTag
import io.getquill.quat._
import io.getquill.Query
import io.getquill.util.Messages
import io.getquill.parser.Lifter
import io.getquill.Udt
import scala.collection.mutable
import io.getquill.generic.GenericEncoder
import io.getquill.generic.GenericDecoder
import io.getquill.util.Format
import io.getquill.generic.DecodingType
import io.getquill.Quoted

inline def quatOf[T]: Quat = ${ QuatMaking.quatOfImpl[T] }

object QuatMaking:
  private class SimpleQuatMaker(using Quotes) extends QuatMakingBase with QuatMaking
  // TODO I don't think anyValBehavior is used anymore, try to remove it
  private def quatMaker(behavior: AnyValBehavior = AnyValBehavior.TreatAsValue)(using qctx: Quotes) =
    new SimpleQuatMaker {
      override def anyValBehavior = behavior
    }

  inline def inferQuat[T](value: T): Quat = ${ inferQuatImpl('value) }
  def inferQuatImpl[T: TType](value: Expr[T])(using quotes: Quotes): Expr[Quat] = {
    val quat = quatMaker().InferQuat.of[T]
    Lifter.quat(quat)
  }

  def ofType[T: TType](using quotes: Quotes): Quat =
    quatMaker().InferQuat.of[T]

  def ofType[T: TType](anyValBehavior: AnyValBehavior)(using quotes: Quotes): Quat =
    quatMaker(anyValBehavior).InferQuat.of[T]

  def quatOfImpl[T: TType](using quotes: Quotes): Expr[Quat] = {
    val quat = quatMaker().InferQuat.of[T]
    Lifter.quat(quat)
  }

  type QuotesTypeRepr = Quotes#reflectModule#TypeRepr

  private val encodableCache: mutable.Map[QuotesTypeRepr, Boolean] = mutable.Map()
  def lookupIsEncodable(tpe: QuotesTypeRepr)(computeEncodable: () => Boolean) =
    val lookup = encodableCache.get(tpe)
    lookup match
      case Some(value) =>
        value
      case None =>
        val encodable = computeEncodable()
        encodableCache.put(tpe, encodable)
        encodable

  private val quatCache: mutable.Map[QuotesTypeRepr, Quat] = mutable.Map()
  def lookupCache(tpe: QuotesTypeRepr)(computeQuat: () => Quat) =
    val lookup = quatCache.get(tpe)
    lookup match
      case Some(value) =>
        value
      case None =>
        val quat = computeQuat()
        quatCache.put(tpe, quat)
        quat

  enum AnyValBehavior:
    case TreatAsValue
    case TreatAsClass
end QuatMaking

trait QuatMaking extends QuatMakingBase:

  override def existsEncoderFor(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
    import quotes.reflect._
    // TODO Try summoning 'value' to know it's a value for sure if a encoder doesn't exist?
    def encoderComputation() = {
      tpe.asType match
        // If an identifier in the Quill query is has a Encoder/Decoder pair, we treat it as a value i.e. Quat.Value is assigned as it's Quat.
        // however, what do we do if there is only one. Say for Name(value: String), Person(name: Name, age: Int) there is a Name-Decoder
        // but no Name-encoder. It is difficult to know whether to treat p.name in `query[Person].map(p => p.name)` as a Quat Value or Product.
        // for this reason, we treat it as a quat-value if there is either a encoder or a decoder and warn the user about it.
        // Furthermore, during various transformation phases, it is possible that a transformation will expect the field of p.name
        // (i.e. `Name.value`) to exist whereas in fact it does not. The transformations recently have been changed to be more tolerant
        // of this possibility but all such cases have not yet been covered. Therefore it is possible that if there is a encoder but not a decoder
        // of a Identifier, an error (e.g. the Quat.Value does not have the field `value` i.e. of `Name.value`) so we warn the user
        // and ask for them to define a Decoder for Name as well.
        // Question: Should we pass in PrepareRow as well in order to have things be possibly products
        // in one dialect and values in another???
        case '[t] =>
          (Expr.summon[GenericEncoder[t, _, _]], Expr.summon[GenericDecoder[_, _, t, DecodingType.Specific]]) match
            case (Some(_), Some(_)) => true
            case (Some(enc), None) =>
              report.warning(
                s"A Encoder:\n" +
                  s"${Format.Expr(enc)}\n" +
                  s"was found for the type ${Format.TypeOf[t]} but not a decoder so this type will " +
                  s"be treated as a value. To avoid potential problems it is preferable to define " +
                  s"both an encoder and a decoder for all types used in Quill Queries."
              )
              true
            case (None, Some(dec)) =>
              report.warning(
                s"A Decoder:\n" +
                  s"${Format.Expr(dec)}\n " +
                  s"was found for the type ${Format.TypeOf[t]} but not a encoder so this type will be " +
                  s"treated as a value. To avoid potential problems it is preferable to define " +
                  s"both an encoder and a decoder for all types used in Quill Queries."
              )
              true
            case (None, None) => false
        case _ =>
          false
    }

    val output = QuatMaking.lookupIsEncodable(tpe.widen)(encoderComputation)
    output
//quotes.reflect.report.throwError(s"No type for: ${tpe}")
end QuatMaking

trait QuatMakingBase:
  import QuatMaking.AnyValBehavior
  def anyValBehavior: AnyValBehavior = AnyValBehavior.TreatAsValue

  // TODO Either can summon an Encoder[T] or quill 'Value[T]' so that we know it's a quat value and not a case class
  def existsEncoderFor(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean

  object InferQuat:
    def of[T](using TType[T], Quotes) =
      ofType(quotes.reflect.TypeRepr.of[T])

    def ofExpr(expr: Expr[Any])(using Quotes) =
      import quotes.reflect._
      ofType(expr.asTerm.tpe)

    def ofType(using Quotes)(tpe: quotes.reflect.TypeRepr): Quat =
      QuatMaking.lookupCache(tpe.widen)(() => ParseType.parseTopLevelType(tpe))

    def nonGenericMethods(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      tpe.classSymbol.get.memberFields
        .filter(m => m.owner.name.toString != "Any" && m.owner.name.toString != "Object").map { param =>
          (
            param.name.toString,
            tpe.memberType(param).simplified

            // Look up the parameter only if needed. This is typically an expensive operation
            // if (!param.isParameter) param.typeSignature else param.typeSignature.asSeenFrom(tpe, tpe.typeSymbol)
          )
        }.toList

    def caseClassConstructorArgs(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import io.getquill.util.Format
      // println(s"For: ${Format.TypeRepr(tpe)} case fields are: ${tpe.classSymbol.get.caseFields.map(p => s"'${p}'").toList}")
      // Note. One one constructor param list is supported due to Quat Generation Specifics. This is already the case in most situations.
      tpe.classSymbol.get.caseFields.map { param =>
        (
          // Not sure why some tuple case methods have spaces... but they do!
          // For: Tuple2[Foo, Ent] case fields are: List('method _1', 'val _1 ', 'method _2', 'val _2 ')
          param.name.toString.trim,
          tpe.memberType(param).simplified
          // if (!param.isParameter) param.typeSignature else param.typeSignature.asSeenFrom(tpe, tpe.typeSymbol)
        )
      }

    object ArbitraryBaseType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[(String, List[(String, quotes.reflect.TypeRepr)])] =
        if (tpe.classSymbol.isDefined)
          Some((tpe.widen.typeSymbol.name.toString, nonGenericMethods(tpe.widen)))
        else
          None

    extension (using Quotes)(sym: quotes.reflect.Symbol)
      def isCaseClass =
        import quotes.reflect._
        sym.caseFields.length > 0

    object CaseClassBaseType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[(String, List[(String, quotes.reflect.TypeRepr)])] =
        if (tpe.classSymbol.isDefined && tpe.widen.typeSymbol.isCaseClass)
          Some((tpe.widen.typeSymbol.name.toString, caseClassConstructorArgs(tpe.widen)))
        else
          None

    object OptionType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
        import quotes.reflect._
        // [Option[t]]  will yield 'Nothing if is pulled out of a non optional value'
        if (tpe.is[Option[_]])
          tpe.asType match
            case '[Option[t]] =>
              Some(TypeRepr.of[t])
            case _ => None
        else
          None

    object Deoption:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
        import quotes.reflect._
        if (isType[Option[_]](tpe))
          tpe.asType match
            case '[Option[t]] => Some(TypeRepr.of[t])
            case _            => Some(tpe)
        else
          Some(tpe)

    def isGeneric(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import quotes.reflect._
      tpe.typeSymbol.isTypeParam ||
      tpe.typeSymbol.isAliasType ||
      tpe.typeSymbol.isAbstractType ||
      tpe.typeSymbol.flags.is(Flags.Trait) ||
      tpe.typeSymbol.flags.is(Flags.Abstract) ||
      tpe.typeSymbol.flags.is(Flags.Param)

    object Param:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
        if (isGeneric(tpe))
          Some(tpe)
        else
          None

    object RealTypeBounds:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
        import quotes.reflect._
        // TypeBounds matcher can throw and exception, need to catch it here
        // so it doesn't blow up compilation
        try {
          tpe match {
            case TypeBounds(lower, upper) =>
              Some((lower, upper))
            case _ =>
              None
          }
        } catch {
          case e => None
        }

    object AnyType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
        import quotes.reflect._
        if (tpe =:= TypeRepr.of[Any] || tpe.widen =:= TypeRepr.of[Any])
          Some(tpe)
        else
          None

    object BooleanType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] =
        import quotes.reflect._
        if (tpe.is[Boolean])
          Some(tpe)
        else
          None

    object ValueType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Quat] =
        import quotes.reflect._
        tpe match
          case AnyType(tpe)                        => Some(Quat.Generic)
          case BooleanType(tpe)                    => Some(Quat.BooleanValue)
          case OptionType(BooleanType(innerParam)) => Some(Quat.BooleanValue)
          case DefiniteValue(tpe)                  => Some(Quat.Value)
          case _                                   => None

    object CaseClassType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Quat] =
        import quotes.reflect._
        tpe match
          case CaseClassBaseType(name, fields) if !existsEncoderFor(tpe) || tpe <:< TypeRepr.of[Udt] =>
            Some(Quat.Product(fields.map { case (fieldName, fieldType) => (fieldName, ParseType.parseType(fieldType)) }))
          case _ =>
            None

    def isConstantType(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import quotes.reflect._
      (tpe.is[Boolean] ||
      tpe.is[String] ||
      tpe.is[Int] ||
      tpe.is[Long] ||
      tpe.is[Float] ||
      tpe.is[Double] ||
      tpe.is[Byte])

    object DefiniteValue {
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
        import quotes.reflect._
        // UDTs (currently only used by cassandra) are created as tables even though there is an encoder for them.
        if (isConstantType(tpe))
          Some(tpe)
        else if (tpe <:< TypeRepr.of[Udt])
          None
        else if (isType[AnyVal](tpe) && tpe.widen.typeSymbol.isCaseClass && anyValBehavior == AnyValBehavior.TreatAsValue)
          Some(tpe)
        else if (existsEncoderFor(tpe))
          Some(tpe)
        else
          None
      }
    }

    object ParseType:
      def parseTopLevelType(using Quotes)(tpe: quotes.reflect.TypeRepr): Quat =
        import quotes.reflect.{Signature => _, _}
        tpe match
          case ValueType(quat) => quat
          // If it is a query type, recurse into it
          case QueryType(tpe) => parseType(tpe)

          // For cases where the type is actually a parameter with type bounds
          // and the upper bound is a trait or abstract class, assume that polymorphism is being used
          // and that the user wants to extend a class e.g.
          // trait Spirit { def grade: Int }
          // case class Gin(grade: Int) extends Spirit
          // def is80Prof[T <: Spirit] = quote { (spirit: Query[Spirit]) => spirit.filter(_.grade == 80) }
          // run(is80Proof(query[Gin]))
          // When processing is80Prof, we assume that Spirit is actually a base class to be extended
          case Param(RealTypeBounds(lower, Deoption(upper))) if (upper.isAbstract && !existsEncoderFor(tpe)) =>
            parseType(upper, true) match {
              case p: Quat.Product => p.copy(tpe = Quat.Product.Type.Abstract)
              case other           => other
            }

          case other => parseType(other)
      end parseTopLevelType

      extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
        def isAbstract =
          import quotes.reflect._
          val flags = tpe.typeSymbol.flags
          flags.is(Flags.Trait) || flags.is(Flags.Abstract)

      /*
       * Quat parsing has a top-level type parsing function and then secondary function which is recursed. This is because
       * things like type boundaries (e.g.  type-bounds types (e.g. Query[T &lt;: BaseType]) should only be checked once
       * at the top level.
       */
      def parseType(using Quotes)(tpe: quotes.reflect.TypeRepr, boundedInterfaceType: Boolean = false): Quat =
        import quotes.reflect._
        tpe match
          case ValueType(quat) => quat

          // This will happens for val-parsing situations e.g. where you have val (a,b) = (Query[A],Query[B]) inside a quoted block.
          // In this situations, the CaseClassBaseType should activate first and recurse which will then hit this case clause.
          case QueryType(tpe) => parseType(tpe)

          // If the type is optional, recurse
          case OptionType(innerParam) => parseType(innerParam)
          case _ if (isNone(tpe))     => Quat.Null

          // For other types of case classes (and if there does not exist an encoder for it)
          // the exception to that is a cassandra UDT that we treat like an encodable entity even if it has a parsed type
          case CaseClassBaseType(name, fields) if !existsEncoderFor(tpe) || tpe <:< TypeRepr.of[Udt] =>
            Quat.Product(fields.map { case (fieldName, fieldType) => (fieldName, parseType(fieldType)) })

          // If we are already inside a bounded type, treat an arbitrary type as a interface list
          case ArbitraryBaseType(name, fields) if (boundedInterfaceType) =>
            Quat.Product(fields.map { case (fieldName, fieldType) => (fieldName, parseType(fieldType)) })

          // If the quat is a coproduct, merge the sub quats that are recursively retrieved
          case CoProduct(quat) => quat

          // Is it a generic or does it have any generic parameters that have not been filled (e.g. is T not filled in Option[T] ?)
          // TODO Improve by making specific flag check to see that it's a coproduct
          case Param(tpe) => Quat.Generic

          // Otherwise it's a terminal value
          case _ =>
            Messages.trace(s"Could not infer SQL-type of ${tpe}, assuming it is a Unknown Quat.")
            Quat.Unknown
        end match
    end ParseType

    object CoProduct:
      import io.getquill.quat.LinkedHashMapOps._
      import scala.deriving._
      import scala.quoted._

      def computeCoproduct[T](using tpe: Type[T])(using Quotes): Option[Quat] = {
        import quotes.reflect._
        Expr.summon[Mirror.Of[T]] match
          case Some(ev) =>
            ev match
              case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
                val coproductQuats = traverseCoproduct[elementTypes](TypeRepr.of[T])(Type.of[elementTypes])
                val reduced = coproductQuats.reduce((q1, q2) => mergeQuats(q1, q2))
                Some(reduced)
              case _ =>
                None

          case None =>
            None
      }

      def isSealedTraitOrEnum(using Quotes)(tpe: quotes.reflect.TypeRepr) =
        import quotes.reflect._
        val flags = tpe.typeSymbol.flags
        (flags.is(Flags.Trait) && flags.is(Flags.Sealed)) || flags.is(Flags.Enum)

      def unapply(using Quotes)(tpeRepr: quotes.reflect.TypeRepr) =
        import quotes.reflect._
        // If you don't widen the exception happens: "Could not match on type: Type.of[...]
        val tpe = tpeRepr.widen.asType
        tpe match
          // Skip optional types, they have a special case
          case _ if (tpeRepr <:< TypeRepr.of[Option[_]]) =>
            None
          // Only allow coproducts that are enums or sealed traits
          case _ if !isSealedTraitOrEnum(tpeRepr.widen) =>
            None
          case '[t] =>
            val typedTpe = tpe.asInstanceOf[Type[t]]
            computeCoproduct[t](using typedTpe)
          case _ =>
            report.throwError(s"Could not match on type: ${tpe}")

      def traverseCoproduct[Types](using Quotes)(parent: quotes.reflect.TypeRepr)(types: Type[Types]): List[Quat] =
        import quotes.reflect._
        types match
          case '[tpe *: tpes] =>
            val quat =
              TypeRepr.of[tpe] match
                case CaseClassType(quat) => quat
                case ValueType(quat)     => quat
                case _ =>
                  report.throwError(
                    s"The Co-Product element ${TypeRepr.of[tpe].show} was not a Case Class or Value Type. Value-level " +
                      s"Co-Products are not supported. Please write a decoder for it's parent-type ${parent.show}."
                  )

            quat :: traverseCoproduct[tpes](parent)(Type.of[tpes])
          case '[EmptyTuple] =>
            Nil

      def mergeQuats(q1: Quat, q2: Quat): Quat =
        (q1, q2) match
          case (first: Quat.Product, second: Quat.Product) =>
            val newFields =
              first.fields.outerZipWith(second.fields) {
                case (key, Some(first), Some(second)) => (key, mergeQuats(first, second))
                case (key, Some(first), None)         => (key, first)
                case (key, None, Some(second))        => (key, second)
                case (key, None, None)                => throw new IllegalArgumentException(s"Invalid state for Quat key ${key}, both values of merging quats were null")
              }
            Quat.Product(newFields)

          case (firstQuat, secondQuat) =>
            firstQuat.leastUpperType(secondQuat) match
              case Some(value) => value
              // TODO Get field names for these quats if they are inside something else?
              case None => throw new IllegalArgumentException(s"Could not create coproduct by merging quats ${q1} and ${q2}")
    end CoProduct

    object QuotedType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
        import quotes.reflect._
        tpe.asType match
          case '[Quoted[t]] => Some(TypeRepr.of[t])
          case _            => None

    object QueryType:
      def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
        import quotes.reflect._
        if (isType[Query[_]](tpe))
          tpe.asType match
            case '[Query[t]] =>
              val out = TypeRepr.of[t]
              Some(out)
            case _ => None
        else
          None

    def isNone(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import quotes.reflect._
      tpe =:= TypeRepr.of[None.type]

    extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
      def is[T](using TType[T]) = isType[T](tpe)

    private[getquill] def isType[T](using Quotes)(tpe: quotes.reflect.TypeRepr)(using tt: TType[T]) =
      import quotes.reflect._
      tpe <:< TypeRepr.of[T] && !(tpe =:= TypeRepr.of[Nothing])

  end InferQuat
end QuatMakingBase
