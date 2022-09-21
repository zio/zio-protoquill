package io.getquill.context.cassandra.encoding

import scala.quoted._
import com.datastax.oss.driver.api.core.data.UdtValue
import scala.collection.mutable.ListBuffer
import io.getquill.generic.ElaborateStructure
import io.getquill.generic.DeconstructElaboratedEntityLevels
import io.getquill.{Udt, UdtMeta}
import io.getquill.generic.ElaborationSide
import io.getquill.context.cassandra.UdtMetaDslMacro
import io.getquill.context.CassandraSession
import com.datastax.oss.driver.api.core.cql.BoundStatement
import scala.reflect.ClassTag
import io.getquill.util.Format
import io.getquill.context.UdtValueLookup
import io.getquill.generic.ElaborateStructure.{ TermType, Leaf, Branch }
import com.datastax.oss.driver.api.core.cql.Row
import io.getquill.util.ThreadUtil
import io.getquill.generic.ConstructType
import io.getquill.generic.ElaborateStructure.UdtBehavior

object UdtDecodingMacro:

  private[UdtDecodingMacro] case class UdtParams[T <: Udt](udt: Expr[UdtValue], meta: Expr[UdtMeta[T]], sess: Expr[UdtValueLookup])

  inline def udtDecoder[Decoder[_], T <: Udt]: CassandraDecoderMaker[Decoder, T] => Decoder[T] =
    ${ udtDecoderImpl[Decoder, T] }

  def udtDecoderImpl[Decoder[_]: Type, T <: Udt: Type](using Quotes): Expr[CassandraDecoderMaker[Decoder, T] => Decoder[T]] = {
    import quotes.reflect._
    import scala.deriving._

    val madeOrFoundMeta = UdtMeta.build[T]
    val decodeUdt = new UdtDecoderMaker[Decoder, T].apply

    if (TypeRepr.of[T] =:= TypeRepr.of[Udt])
      report.warning(s"Attempting to make UDT decoder for from ${Format.TypeOf[Decoder[T]]}:\n${ThreadUtil.currentThreadTrace}")

    '{
      (cem: CassandraDecoderMaker[Decoder, T]) => {
        val meta = ${madeOrFoundMeta}
        cem.apply ((i: Int, row: Row, sess: UdtValueLookup) => {
          val udtValue = row.getUdtValue(i)
          ${ decodeUdt(UdtParams('udtValue, 'meta, 'sess)) }
        } )
      }
    }
  }

  inline def udtDecodeMapper[Encoder[_], T <: Udt]: CassandraDecodeMapperMaker[Encoder, T] => CassandraMapper[UdtValue, T, MapperSide.Decode] = ${ udtDecodeMapperImpl[Encoder, T] }

  def udtDecodeMapperImpl[Encoder[_]: Type, T <: Udt: Type](using Quotes): Expr[CassandraDecodeMapperMaker[Encoder, T] => CassandraMapper[UdtValue, T, MapperSide.Decode]] =
    import quotes.reflect._
    val madeOrFoundMeta = UdtMeta.build[T]

    // TODO quill.trace.types 'summoning' level should enable this
    //println(s"**** Mapper summoning decode of: ${Format.TypeOf[T]}")

    val decodeUdt = new UdtDecoderMaker[Encoder, T].apply
    '{
      (cem: CassandraDecodeMapperMaker[Encoder, T]) => {
        val meta = ${madeOrFoundMeta}
        cem.apply((udtValue, sess) => {
          val udt = sess.udtValueOf(meta.name, meta.keyspace)
          ${ decodeUdt(UdtParams('udtValue, 'meta, 'sess)) }
        })
      }
    }

  class UdtDecoderMaker[Encoder[_]: Type, T <: Udt: Type](using Quotes):
    import quotes.reflect._

    def apply: UdtParams[T] => Expr[T] =
      (info: UdtParams[T]) => {
        // TODO Shared between encoder and decoder. Extract
        def lookupField(name: String) =
          '{ ${info.meta}.alias(${Expr(name)}).getOrElse(${Expr(name)}) }

        def getField[C: Type](udtValue: Expr[UdtValue], fieldName: String, mapper: Expr[CassandraMapper[_, C, MapperSide.Decode]]) =
          mapper.asTerm.tpe.asType match
            case '[CassandraMapper[fromT, C, MapperSide.Decode]] =>
              val lookedUpField = lookupField(fieldName)
              '{
                val classTag = ${summonClassTagOrFail[fromT]}.runtimeClass.asInstanceOf[Class[fromT]]
                val fieldValue = $udtValue.get[fromT]($lookedUpField, classTag)
                $mapper.asInstanceOf[CassandraMapper[fromT, C, MapperSide.Decode]].f(fieldValue, ${info.sess})
              }

        // Elem is the elem type of the encoder. C is the component (i.e. udt-field) type
        def getOptional[C: Type](udtValue: Expr[UdtValue], fieldName: String) =
          '{ Option(${ getField[C](udtValue, fieldName, summonMapperOrFail[C]) }) }

        def getRegular[C: Type](udtValue: Expr[UdtValue], fieldName: String) =
          getField[C](udtValue, fieldName, summonMapperOrFail[C])

        // TODO Try swapping out all asInstanceOf for asExprOf outside the expression
        def getList[C: Type](udtValue: Expr[UdtValue], fieldName: String) =
          val lookedUpField = lookupField(fieldName)
          val mapper = summonMapperOrFail[C]
          mapper.asTerm.tpe.asType match
            case '[CassandraMapper[fromT, C, MapperSide.Decode]] =>
              val typedMapper = '{ $mapper.asInstanceOf[CassandraMapper[fromT, C, MapperSide.Decode]] }
              val classFromT = '{ ${summonClassTagOrFail[fromT]}.runtimeClass.asInstanceOf[Class[fromT]] }
              '{ UdtValueOps($udtValue)
                .getScalaList[fromT]($lookedUpField, $classFromT)
                .map(row => $typedMapper.f(row, ${info.sess}))
                .toList
              }

        def getSet[C: Type](udtValue: Expr[UdtValue], fieldName: String) =
          val lookedUpField = lookupField(fieldName)
          val mapper = summonMapperOrFail[C]
          mapper.asTerm.tpe.asType match
            case '[CassandraMapper[fromT, C, MapperSide.Decode]] =>
              val typedMapper = '{ $mapper.asInstanceOf[CassandraMapper[fromT, C, MapperSide.Decode]] }
              val classFromT = '{ ${summonClassTagOrFail[fromT]}.runtimeClass.asInstanceOf[Class[fromT]] }
              '{ UdtValueOps($udtValue)
                .getScalaSet[fromT]($lookedUpField, $classFromT)
                .map(row => $typedMapper.f(row, ${info.sess}))
                .toSet
              }

        def getMap[CK: Type, CV: Type](udtValue: Expr[UdtValue], fieldName: String) =
          val lookedUpField = lookupField(fieldName)
          val keyMapper = summonMapperOrFail[CK]
          val valMapper = summonMapperOrFail[CV]
          (keyMapper.asTerm.tpe.asType, valMapper.asTerm.tpe.asType) match
            case ('[CassandraMapper[fromKT, CK, MapperSide.Decode]], '[CassandraMapper[fromVT, CV, MapperSide.Decode]]) =>
              val typedKeyMapper = '{ $keyMapper.asInstanceOf[CassandraMapper[fromKT, CK, MapperSide.Decode]] }
              val typedValMapper = '{ $valMapper.asInstanceOf[CassandraMapper[fromVT, CV, MapperSide.Decode]] }
              val classFromKT = '{ ${summonClassTagOrFail[fromKT]}.runtimeClass.asInstanceOf[Class[fromKT]] }
              val classFromVT = '{ ${summonClassTagOrFail[fromVT]}.runtimeClass.asInstanceOf[Class[fromVT]] }
              '{ UdtValueOps($udtValue)
                .getScalaMap[fromKT, fromVT]($lookedUpField, $classFromKT, $classFromVT)
                .map[CK, CV](row =>
                  ($typedKeyMapper.f(row._1, ${info.sess}), $typedValMapper.f(row._2, ${info.sess}))
                ).toMap
              }

        def deriveComponents =
          val (mirror, mirrorFields) = MirrorFields.of[T]
          val mappedFields =
            mirrorFields.map {
              case (fieldName, t @ '[List[tpe]]) =>
                (t, getList[tpe](info.udt, fieldName))
              case (fieldName, t @ '[Set[tpe]]) =>
                (t, getSet[tpe](info.udt, fieldName))
              case (fieldName, t @ '[Map[tpeK, tpeV]]) =>
                (t, getMap[tpeK, tpeV](info.udt, fieldName))
              case (fieldName, t @ '[Option[tpe]]) =>
                (t, getOptional[tpe](info.udt, fieldName))
              case (fieldName, t @ '[tpe]) =>
                (t, getRegular[tpe](info.udt, fieldName))
            }
          (mirror, mappedFields)

        val (mirror, mappedFields) = deriveComponents
        val out = ConstructType(mirror, mappedFields)
        out
      }
    end apply

    def summonClassTagOrFail[CT: Type] =
        Expr.summon[ClassTag[CT]] match
          case Some(ct) => ct
          case None =>
            report.throwError(s"Error creating Encoder[${Format.TypeOf[T]}]. Cannot summon ClassTag for ${Format.TypeOf[CT]}")

    def summonMapperOrFail[MT: Type]: Expr[CassandraMapper[_, MT, MapperSide.Decode]] =
      import quotes.reflect._
      Expr.summon[CassandraMapper[_, MT, MapperSide.Decode]] match
        case Some(cm) => cm
        case None =>
          report.throwError(s"Error creating Encoder[${Format.TypeOf[T]}]. Cannot summon a CassandraMapper[${Format.TypeOf[MT]}, _, ${Format.TypeOf[MapperSide.Decode]}]")
  end UdtDecoderMaker

end UdtDecodingMacro


object UdtEncodingMacro:

  private[UdtEncodingMacro] case class UdtParams[T <: Udt: Type](elem: Expr[T], udt: Expr[UdtValue], meta: Expr[UdtMeta[T]], sess: Expr[UdtValueLookup])

  class UdtEncoderMaker[Encoder[_]: Type, T <: Udt: Type](using Quotes):
    import quotes.reflect._

    def apply: (UdtParams[T] => Expr[UdtValue], TermType) = {
      val (deconstructedEntityComponents, elaborationType) = ElaborateStructure.decomposedProductValueDetails[T](ElaborationSide.Encoding, UdtBehavior.Derive)
      // val ents = deconstructedEntityComponents.map { case (t, o, g, tpe) => s"(${t} --> ${Format.Expr(g)})"}
      //println(s"Components of: ${Format.TypeOf[T]}: ${ents}" )

      val udtMaker =
        (info: UdtParams[T]) => {
          def lookupField(name: String) =
            '{ ${info.meta}.alias(${Expr(name)}).getOrElse(${Expr(name)}) } // todo insert summoned naming strategy here

          def setField[C: Type](fieldValue: Expr[C], fieldName: String, udt: Expr[UdtValue], mapper: Expr[CassandraMapper[C, _, MapperSide.Encode]]) =
            mapper.asTerm.tpe.asType match
              case '[CassandraMapper[C, toT, MapperSide.Encode]] =>
                val lookedUpField = lookupField(fieldName)
                '{ $udt.set[toT](
                    $lookedUpField,
                    $mapper.asInstanceOf[CassandraMapper[C, toT, MapperSide.Encode]].f($fieldValue, ${info.sess}),
                    ${summonClassTagOrFail[toT]}.runtimeClass.asInstanceOf[Class[toT]])
                }

          // Elem is the elem type of the encoder. C is the component (i.e. udt-field) type
          def setOptional[Elem: Type, C: Type](fieldValue: Expr[Elem], fieldName: String, udt: Expr[UdtValue], getter: Expr[Elem] => Expr[?]) =
            '{
              ${getter.apply(fieldValue)}.asInstanceOf[Option[C]].map(v =>
                ${ setField[C]('v, fieldName, udt, summonMapperOrFail[C]) }
              ).getOrElse($udt.setToNull(${lookupField(fieldName)}))
            }

          def setRegular[Elem: Type, C: Type](fieldValue: Expr[Elem], fieldName: String, udt: Expr[UdtValue], getter: Expr[Elem] => Expr[?]) =
            val v = '{ ${getter.apply(fieldValue)}.asInstanceOf[C] }
            setField(v, fieldName, udt, summonMapperOrFail[C])

          // TODO Try swapping out all asInstanceOf for asExprOf outside the expression
          def setList[Elem: Type, C: Type](fieldValue: Expr[Elem], fieldName: String, udt: Expr[UdtValue], getter: Expr[Elem] => Expr[?]) =
            val lookedUpField = lookupField(fieldName)
            val mapper = summonMapperOrFail[C]
            mapper.asTerm.tpe.asType match
              case '[CassandraMapper[C, toT, MapperSide.Encode]] =>
                val typedMapper = '{ $mapper.asInstanceOf[CassandraMapper[C, toT, MapperSide.Encode]] }
                val list = '{ ${getter.apply(fieldValue)}.asInstanceOf[List[C]].map(row => $typedMapper.f(row, ${info.sess})) }
                val classToT = '{ ${summonClassTagOrFail[toT]}.runtimeClass.asInstanceOf[Class[toT]] }
                '{ UdtValueOps($udt).setScalaList[toT]($lookedUpField, $list, $classToT) }

          def setSet[Elem: Type, C: Type](fieldValue: Expr[Elem], fieldName: String, udt: Expr[UdtValue], getter: Expr[Elem] => Expr[?]) =
            val lookedUpField = lookupField(fieldName)
            val mapper = summonMapperOrFail[C]
            mapper.asTerm.tpe.asType match
              case '[CassandraMapper[C, toT, MapperSide.Encode]] =>
                val typedMapper = '{ $mapper.asInstanceOf[CassandraMapper[C, toT, MapperSide.Encode]] }
                val set = '{ ${getter.apply(fieldValue)}.asInstanceOf[Set[C]].map(row => $typedMapper.f(row, ${info.sess})) }
                val classToT = '{ ${summonClassTagOrFail[toT]}.runtimeClass.asInstanceOf[Class[toT]] }
                '{ UdtValueOps($udt).setScalaSet[toT]($lookedUpField, $set, $classToT) }

          def setMap[Elem: Type, CK: Type, CV: Type](fieldValue: Expr[Elem], fieldName: String, udt: Expr[UdtValue], getter: Expr[Elem] => Expr[?]) =
            val lookedUpField = lookupField(fieldName)
            val keyMapper = summonMapperOrFail[CK]
            val valMapper = summonMapperOrFail[CV]
            (keyMapper.asTerm.tpe.asType, valMapper.asTerm.tpe.asType) match
              case ('[CassandraMapper[CK, toKT, MapperSide.Encode]], '[CassandraMapper[CV, toVT, MapperSide.Encode]]) =>
                val typedKeyMapper = '{ $keyMapper.asInstanceOf[CassandraMapper[CK, toKT, MapperSide.Encode]] }
                val typedValMapper = '{ $valMapper.asInstanceOf[CassandraMapper[CV, toVT, MapperSide.Encode]] }
                val map =
                  '{ ${getter.apply(fieldValue)}.asInstanceOf[Map[CK, CV]]
                      .map[toKT, toVT](kv => ($typedKeyMapper.f(kv._1, ${info.sess}), $typedValMapper.f(kv._2, ${info.sess})))
                  }
                val classToKT = '{ ${summonClassTagOrFail[toKT]}.runtimeClass.asInstanceOf[Class[toKT]] }
                val classToVT = '{ ${summonClassTagOrFail[toVT]}.runtimeClass.asInstanceOf[Class[toVT]] }
                '{ UdtValueOps($udt).setScalaMap[toKT, toVT]($lookedUpField, $map, $classToKT, $classToVT) }

          val components =
            deconstructedEntityComponents.map {
              case (fieldName, isOptional, getter, fieldType) =>
                fieldType match
                  case '[List[tpe]] =>
                    setList[T, tpe](info.elem, fieldName, info.udt, getter)
                  case '[Set[tpe]] =>
                    setSet[T, tpe](info.elem, fieldName, info.udt, getter)
                  case '[Map[tpeK, tpeV]] =>
                    setMap[T, tpeK, tpeV](info.elem, fieldName, info.udt, getter)
                  case '[tpe] =>
                    if (isOptional)
                      setOptional[T, tpe](info.elem, fieldName, info.udt, getter)
                    else
                      setRegular[T, tpe](info.elem, fieldName, info.udt, getter)
            }

          if (components.isEmpty)
            report.throwError(s"The udt-type ${Format.TypeOf[T]} does not have any fields")

          val otherCalls = components.dropRight(1)
          val lastCall = components.last
          Block(otherCalls.map(_.asTerm), lastCall.asTerm).asExprOf[UdtValue]
        }

      (udtMaker, elaborationType)
    }

    def summonClassTagOrFail[CT: Type] =
        Expr.summon[ClassTag[CT]] match
          case Some(ct) => ct
          case None =>
            report.throwError(s"Error creating Encoder[${Format.TypeOf[T]}]. Cannot summon ClassTag for ${Format.TypeOf[CT]}")

    def summonMapperOrFail[MT: Type]: Expr[CassandraMapper[MT, _, MapperSide.Encode]] =
      import quotes.reflect._
      Expr.summon[CassandraMapper[MT, _, MapperSide.Encode]] match
        case Some(cm) => cm
        case None =>
          report.throwError(s"Error creating Encoder[${Format.TypeOf[T]}]. Cannot summon a CassandraMapper[${Format.TypeOf[MT]}, _, ${Format.TypeOf[MapperSide.Encode]}]")



  end UdtEncoderMaker

  // Assuming that:
  // PrepareRow := BoundStatement, Session := CassandraSession
  inline def udtEncoder[Encoder[_], T <: Udt]: CassandraEncoderMaker[Encoder, T] => Encoder[T] =
    ${ udtEncoderImpl[Encoder, T] }

  /**
   * Generate an Encoder[T] for some arbitrary Encoder[_] and arbitrary T <: Udt. This
   * is done via the Generic Derivation mechanisms of the ProtoQuill Elaborators.
   *
   * Since we can't directly return an Encoder due to https://github.com/lampepfl/dotty/issues/12179
   * (have a look at the AnyVal encoder for more info), we pass it in as a parameter and the
   * the CassandraEncoderMaker pattern to generate it. If we assume that all the encoders cassandra
   * contexts ever need to use are CassandraEncoder instances then we don't need this parameter
   * and maybe can simplify. Need to look into this some more.
   */
  def udtEncoderImpl[Encoder[_]: Type, T <: Udt: Type](using Quotes): Expr[CassandraEncoderMaker[Encoder, T] => Encoder[T]] = {
    import quotes.reflect._
    import scala.deriving._

    val madeOrFoundMeta = UdtMeta.build[T]
    val (encodeUdt, elaborationType) = new UdtEncoderMaker[Encoder, T].apply

    def synthesizeEncoder: Expr[CassandraEncoderMaker[Encoder, T] => Encoder[T]] =
      '{
        (cem: CassandraEncoderMaker[Encoder, T]) => {
          val meta = ${madeOrFoundMeta}
          cem.apply ((i: Int, elem: T, row: BoundStatement, sess: UdtValueLookup) => row.setUdtValue(i, {
            val udt = sess.udtValueOf(meta.name, meta.keyspace)
            ${ encodeUdt(UdtParams('elem, 'udt, 'meta, 'sess)) }
          }))
        }
      }

    elaborationType match
      case Leaf =>
        Expr.summon[Encoder[T]] match
          // If encoder has already been synthesized by a different macro invocation, elaboration
          // of the encoder will be a Leaf since an encoder exists for it. In that case we just summon the encoder
          case Some(value) =>
            '{ (cem: CassandraEncoderMaker[Encoder, T]) => $value }
          case None =>
            println(s"[ERROR] Could not synthesize leaf UDT encoder for: ${Format.TypeOf[T]}")
            '{ ??? }
      case Branch =>
        synthesizeEncoder
  }

  inline def udtEncoderMapper[Encoder[_], T <: Udt]: CassandraEncodeMapperMaker[Encoder, T] => CassandraMapper[T, UdtValue, MapperSide.Encode] = ${ udtEncoderMapperImpl[Encoder, T] }

  def udtEncoderMapperImpl[Encoder[_]: Type, T <: Udt: Type](using Quotes): Expr[CassandraEncodeMapperMaker[Encoder, T] => CassandraMapper[T, UdtValue, MapperSide.Encode]] = {
    import quotes.reflect._
    val madeOrFoundMeta = UdtMeta.build[T]
    // TODO quill.trace.types 'summoning' level should enable this
    //println(s"**** Mapper summoning encoder of: ${Format.TypeOf[T]}")
    val (encodeUdt, _) = new UdtEncoderMaker[Encoder, T].apply
    '{
      (cem: CassandraEncodeMapperMaker[Encoder, T]) => {
        val meta = ${madeOrFoundMeta}
        cem.apply((elem, sess) => {
          val udt = sess.udtValueOf(meta.name, meta.keyspace)
          ${ encodeUdt(UdtParams('elem, 'udt, 'meta, 'sess)) }
        })
      }
    }
  }

end UdtEncodingMacro
