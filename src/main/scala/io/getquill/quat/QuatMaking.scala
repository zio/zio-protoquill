package io.getquill.quat

import scala.quoted.{ Type => TType, _ }

// import io.getquill.Udt
import scala.annotation.tailrec
import scala.reflect.ClassTag
import io.getquill.quat._
import io.getquill.Query
import io.getquill.util.Messages
import io.getquill.parser.Lifter
import io.getquill.Udt

case class Quoted[+T](val ast: io.getquill.ast.Ast)

trait Value[T]

// TODO Quat lifting so can return them from this function

object QuatMaking {
  inline def inferQuat[T](value: T): Quat = ${ inferQuatImpl('value) }
  def inferQuatImpl[T: TType](value: Expr[T])(using quotes: Quotes): Expr[Quat] = {
    val quat = (new QuatMaking {}).InferQuat.of[T]
    println(io.getquill.util.Messages.qprint(quat))
    Lifter.quat(quat)
  }

  def ofType[T: TType](using quotes: Quotes): Expr[Quat] = {
    val quat = (new QuatMaking {}).InferQuat.of[T]
    println(io.getquill.util.Messages.qprint(quat))
    Lifter.quat(quat)
  }
}

inline def quatOf[T]: Quat = ${ QuatMaking.ofType[T] }

trait QuatMaking(using override val qctx: Quotes) extends QuatMakingBase {
  import qctx.reflect._
  override def existsEncoderFor(tpe: TypeRepr): Boolean =  
    tpe.asType match
      case '[t] => Expr.summon[Value[t]] match
        case Some(_) => true
        case None => false
      case _ =>
        false
        //quotes.reflect.report.throwError(s"No type for: ${tpe}")
}


trait QuatMakingBase(using val qctx: Quotes) {
  import qctx.reflect._

  // TODO Either can summon an Encoder[T] or quill 'Value[T]' so that we know it's a quat value and not a case class
  def existsEncoderFor(tpe: TypeRepr): Boolean

  object InferQuat:
    def of[T](using TType[T]) = ofType(TypeRepr.of[T])
    def ofExpr(expr: Expr[Any]) = ofType(expr.asTerm.tpe)

    def ofType(tpe: TypeRepr): Quat = {

      def nonGenericMethods(tpe: TypeRepr) = {
        tpe.classSymbol.get.memberFields
          .filter(m => m.owner.name.toString != "Any" && m.owner.name.toString != "Object").map { param => 
            (
              param.name.toString,
              tpe.memberType(param).simplified

              // Look up the parameter only if needed. This is typically an expensive operation
              //if (!param.isParameter) param.typeSignature else param.typeSignature.asSeenFrom(tpe, tpe.typeSymbol)
            )
          }.toList
      }

      def caseClassConstructorArgs(tpe: TypeRepr) = {
        // Note. One one constructor param list is supported due to Quat Generation Specifics. This is already the case in most situations.
        tpe.classSymbol.get.caseFields.map { param =>
          (
            param.name.toString,
            tpe.memberType(param).simplified
            //if (!param.isParameter) param.typeSignature else param.typeSignature.asSeenFrom(tpe, tpe.typeSymbol)
          )
        }
      }

      object ArbitraryBaseType {
        def unapply(tpe: TypeRepr): Option[(String, List[(String, TypeRepr)])] =
          if (tpe.classSymbol.isDefined)
            Some((tpe.widen.typeSymbol.name.toString, nonGenericMethods(tpe.widen)))
          else
            None
      }

      extension (sym: Symbol)
        def isCaseClass = sym.caseFields.length > 0

      object CaseClassBaseType {
        def unapply(tpe: TypeRepr): Option[(String, List[(String, TypeRepr)])] =
          if (tpe.classSymbol.isDefined && tpe.widen.typeSymbol.isCaseClass)
            Some((tpe.widen.typeSymbol.name.toString, caseClassConstructorArgs(tpe.widen)))
          else
            None
      }

      object Signature {
        def unapply(tpe: TypeRepr) =
          Some(tpe.typeSymbol)
      }

      object OptionType {
        def unapply(tpe: TypeRepr): Option[TypeRepr] =
          // [Option[t]]  will yield 'Nothing if is pulled out of a non optional value'
          if (tpe.is[Option[_]])
            tpe.asType match
              case '[Option[t]] => 
                //println(s"**************** Pulling Out Inner Value: ${TypeRepr.of[t]}")
                Some(TypeRepr.of[t])
              case _ => None
          else
            None
      }

      object Deoption {
        def unapply(tpe: TypeRepr): Option[TypeRepr] =
          if (isType[Option[_]](tpe))
            tpe.asType match
              case '[Option[t]] => Some(TypeRepr.of[t])
              case _ => Some(tpe)
          else
            Some(tpe)
      }

      def isGeneric(tpe: TypeRepr) = {
        // Note that things like String are alias types so can't
        // use this functionality when checking against constants
        // if (tpe.typeSymbol.isTypeParam) println(s"((((( ${tpe} is a Type Param")
        // if (tpe.typeSymbol.isAliasType) println(s"((((( ${tpe} is a Alias Type")
        // if (tpe.typeSymbol.isAbstractType) println(s"((((( ${tpe} is a Abstract Type")
        // TODO What about abstract classes? What does Flags.Abstract do?
        tpe.typeSymbol.isTypeParam || tpe.typeSymbol.isAliasType || tpe.typeSymbol.isAbstractType || tpe.typeSymbol.flags.is(Flags.Trait) || tpe.typeSymbol.flags.is(Flags.Abstract) || tpe.typeSymbol.flags.is(Flags.Param)
      }
      
      object Param {
        def unapply(tpe: TypeRepr) =
          if (isGeneric(tpe))
            Some(tpe)
          else
            None
      }

      object RealTypeBounds {
        def unapply(tpe: TypeRepr) =
          // TypeBounds matcher can throw and exception, need to catch it here
          // so it doesn't blow up compilation
          try {
            tpe match {
              case TypeBounds(lower, upper)  => //if (upper != null && !(upper =:= TypeRepr.of[Any]))
                //println(s"@@@@@@@@@@@@@@@ Type Bounds ${lower} <:< ${upper}")
                Some((lower, upper))
              case _ =>
                None
            }
          } catch {
            case e => None
          }
      }

      object AnyType {
        def unapply(tpe: TypeRepr): Option[TypeRepr] =
          if (tpe =:= TypeRepr.of[Any] || tpe.widen =:=  TypeRepr.of[Any])
            Some(tpe)
          else
            None
      }

      object BooleanType {
        def unapply(tpe: TypeRepr): Option[TypeRepr] =
          if (tpe.is[Boolean])
            Some(tpe)
          else
            None
      }

      def isConstantType(tpe: TypeRepr) =
        (tpe.is[Boolean] ||
        tpe.is[String] ||
        tpe.is[Int] ||
        tpe.is[Long] ||
        tpe.is[Float] ||
        tpe.is[Double] ||
        tpe.is[Byte])

      object DefiniteValue {
        def unapply(tpe: TypeRepr): Option[TypeRepr] = {
          // UDTs (currently only used by cassandra) are created as tables even though there is an encoder for them.
          if (isConstantType(tpe))
            //println(s"------------> ${tpe} Is constant type ")
            Some(tpe)
          else if (tpe <:< TypeRepr.of[Udt])
            //println(s"------------> ${tpe} Is UDT (not definite value) ")
            None
          else if (isType[AnyVal](tpe))
            //println(s"------------> ${tpe} Is AnyValue")
            Some(tpe)
          else if (existsEncoderFor(tpe))
            //println(s"------------> ${tpe} Has Encoder")
            Some(tpe)
          else
            None
        }
      }

      def parseTopLevelType(tpe: TypeRepr): Quat =
        // println(s"================ TOP LEVEL Type: ${tpe} ================")
        // println(s"Simplified Type: ${tpe.simplified}")
        // println(s"Widened Type: ${tpe.widen}")
        // println(s"isBoolean: ${BooleanType.unapply(tpe).isDefined}")
        // println(s"Is Constant: ${tpe match {case c: Constant => true; case _ => false} }")
        // println(s"Is Constant Type: ${tpe match {case c: ConstantType => true; case _ => false} }")
        tpe match {
          case AnyType(tpe) =>
            Quat.Generic

          case BooleanType(tpe) =>
            //println("========> TOP LEVEL Boolean")
            Quat.BooleanValue

          case OptionType(BooleanType(innerParam)) =>
            //println("========> TOP LEVEL Boolean")
            Quat.BooleanValue

          case DefiniteValue(tpe) =>
            //println("========> TOP LEVEL Value")
            Quat.Value

          // If it is a query type, recurse into it
          case QueryType(tpe) =>
            //println("========> TOP LEVEL Query")
            parseType(tpe)

          // For cases where the type is actually a parameter with type bounds
          // and the upper bound is not final, assume that polymorphism is being used
          // and that the user wants to extend a class e.g.
          // trait Spirit { def grade: Int }
          // case class Gin(grade: Int) extends Spirit
          // def is80Prof[T <: Spirit] = quote { (spirit: Query[Spirit]) => spirit.filter(_.grade == 80) }
          // run(is80Proof(query[Gin]))
          // When processing is80Prof, we assume that Spirit is actually a base class to be extended
          
          // TODO any way in dotty to find out if a class is final?
          case Param(Signature(RealTypeBounds(lower, Deoption(upper)))) if (/*!upper.typeSymbol.isFinal &&*/ !existsEncoderFor(tpe)) =>
            //println("========> TOP LEVEL Type Bound")
            parseType(upper, true) match {
              case p: Quat.Product => p.copy(tpe = Quat.Product.Type.Abstract)
              case other           => other
            }

          // TODO any way in dotty to find out if a class is final?
          case Param(RealTypeBounds(lower, Deoption(upper))) if (/*!upper.typeSymbol.isFinal && */ !existsEncoderFor(tpe)) =>
            //println("========> TOP LEVEL Deoption Type Bound")
            parseType(upper, true) match {
              // TODO Put back after 3.6.0 release that actually has Quat.Product.Type.Abstract
              //case p: Quat.Product => p.copy(tpe = Quat.Product.Type.Abstract)
              case other           => other
            }

          case other =>
            //println("========> NOT TOP LEVEL")
            parseType(other)
        }

      /*
      * Quat parsing has a top-level type parsing function and then secondary function which is recursed. This is because
      * things like type boundaries (e.g.  type-bounds types (e.g. Query[T &lt;: BaseType]) should only be checked once
      * at the top level.
      */
      def parseType(tpe: TypeRepr, boundedInterfaceType: Boolean = false): Quat =
        //println(s"================ Type: ${tpe} ================")
        // println(s"Simplified Type: ${tpe.simplified}")
        // println(s"Widened Type: ${tpe.widen}")
        // println(s"isBoolean: ${BooleanType.unapply(tpe).isDefined}")
        // println(s"Is Constant: ${tpe match {case c: Constant => true; case _ => false} }")
        // println(s"Is Constant Type: ${tpe match {case c: ConstantType => true; case _ => false} }")
        tpe match {
          case AnyType(tpe) =>
            Quat.Generic

          case BooleanType(tpe) =>
            //println(s"=========> ${tpe} Is Boolean")
            Quat.BooleanValue

          case OptionType(BooleanType(_)) =>
            //println(s"=========> ${tpe} Is Option Of Boolean")
            Quat.BooleanValue

          case DefiniteValue(tpe) =>
            //println("=========> Is Value")
            Quat.Value

          // This will happens for val-parsing situations e.g. where you have val (a,b) = (Query[A],Query[B]) inside a quoted block.
          // In this situations, the CaseClassBaseType should activate first and recurse which will then hit this case clause.
          case QueryType(tpe) =>
            //println("=========> Is Query")
            parseType(tpe)

          // If the type is optional, recurse
          case OptionType(innerParam) =>
            //println("=========> Is Option")
            parseType(innerParam)

          case _ if (isNone(tpe)) =>
            //println("=========> Is Null")
            Quat.Null

          // For other types of case classes (and if there does not exist an encoder for it)
          // the exception to that is a cassandra UDT that we treat like an encodeable entity even if it has a parsed type
          case CaseClassBaseType(name, fields) if !existsEncoderFor(tpe) || tpe <:< TypeRepr.of[Udt] =>
            //println("=========> Is CaseClassBased")
            Quat.Product(fields.map { case (fieldName, fieldType) => (fieldName, parseType(fieldType)) })

          // If we are already inside a bounded type, treat an arbitrary type as a interface list
          case ArbitraryBaseType(name, fields) if (boundedInterfaceType) =>
            //println("=========> Is ArbitraryClassBased")
            Quat.Product(fields.map { case (fieldName, fieldType) => (fieldName, parseType(fieldType)) })

          // Is it a generic or does it have any generic parameters that have not been filled (e.g. is T not filled in Option[T] ?)
          case Param(tpe) =>
            //println("=========> Is Generic")
            Quat.Generic

          // Otherwise it's a terminal value
          case _ =>
            // TODO Put back once release 3.6.0 which will have unknown quats
            Quat.Unknown
            //Messages.trace(s"Could not infer SQL-type of ${tpe}, assuming it is a Unknown Quat.")
            //Quat.Unknown
        }

      val output = parseTopLevelType(tpe)
      //println(s"*********** PARSED QUAT: ${output} ***********")
      output
    }

    object QuotedType {
      def unapply(tpe: TypeRepr) =
        tpe.asType match
          case '[Quoted[t]] => Some(TypeRepr.of[t])
          case _ => None
    }

    object QueryType {
      def unapply(tpe: TypeRepr) =
        if (isType[Query[_]](tpe))
          tpe.asType match
            case '[Query[t]] => 
              val out = TypeRepr.of[t]
              //println(s"&&&&&&&&&&& DeQuery: ${out}")
              //println(s"Subtype of bool: ${out <:< TypeRepr.of[Boolean]}")
              Some(out)
            case _ => None
        else
          None
    }

    def isNone(tpe: TypeRepr) = {
      val era = tpe//.erasure
      era =:= TypeRepr.of[None.type]
    }

    extension [T: TType] (tpe: TypeRepr)
      def is = isType[T](tpe)

    private[getquill] def isType[T](tpe: TypeRepr)(implicit tt: TType[T]) =
      tpe <:< TypeRepr.of[T] && !(tpe =:= TypeRepr.of[Nothing])

  end InferQuat
}
