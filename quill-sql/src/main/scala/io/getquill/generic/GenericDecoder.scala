package io.getquill.generic

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}
import io.getquill.metaprog.TypeExtensions._
import io.getquill.util.Format

trait GenericColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

sealed trait DecodingType
object DecodingType {
  sealed trait Generic extends DecodingType
  sealed trait Specific extends DecodingType
}

trait GenericDecoder[ResultRow, T, +DecType <: DecodingType] extends ((Int, ResultRow) => T) {
  def apply(i: Int, rr: ResultRow): T
}

trait GenericRowTyper[ResultRow, Co] {
  def apply(rr: ResultRow): ClassTag[_]
}

object GenericDecoder {

  def resolveIndexOrFallback[ResultRow: Type](originalIndex: Expr[Int], resultRow: Expr[ResultRow], fieldName: String)(using Quotes) =
    import quotes.reflect._
    Expr.summon[GenericColumnResolver[ResultRow]] match
      case Some(resolver) => '{ $resolver($resultRow, ${Expr(fieldName)}) }
      case None => originalIndex

  def summonAndDecode[ResultRow: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using Quotes): Expr[T] =
    import quotes.reflect.{Term => QTerm, _}
    // Try to summon a specific decoder, if it's not there, summon a generic one
    Expr.summon[GenericDecoder[ResultRow, T, DecodingType.Specific]] match
      case Some(decoder) => 
        '{ $decoder($index, $resultRow) }
      case None =>
        Expr.summon[GenericDecoder[ResultRow, T, DecodingType.Generic]] match
          case Some(decoder) => '{ $decoder($index, $resultRow) }
          case _ => 
            println(s"WARNING Could not summon a decoder for the type: ${io.getquill.util.Format.TypeOf[T]}")
            report.throwError(s"Cannot find decoder for the type: ${Format.TypeOf[T]}")

  def flatten[ResultRow: Type, Fields: Type, Types: Type](index: Expr[Int], resultRow: Expr[ResultRow])(fieldsTup: Type[Fields], typesTup: Type[Types])(using Quotes): List[(Type[_], Expr[_])] = {
    import quotes.reflect.{Term => QTerm, _}

    (fieldsTup, typesTup) match {
      // Check if the field has a specific user-defined encoder for it e.g. Person(name: String) (i.e. since there is a String encoder)
      // or if it is an embedded entity e.g. Person(name: Name, age: Int) where Name is `case class Name(first: String, last: String)`.
      // TODO summoning `GenericDecoder[ResultRow, T, DecodingType.Specific]` here twice, once in the if statement,
      // then later in summonAndDecode. This can potentially be improved i.e. it can be summoned just once and reused.
      case ('[field *: fields], '[tpe *: types]) if Expr.summon[GenericDecoder[ResultRow, tpe, DecodingType.Specific]].isEmpty =>
        // If it is a generic inner class, find out by how many columns we have to move forward
        // An alternative to this is the decoder returning the index incremented after the last thing it decoded
       val air = TypeRepr.of[tpe].classSymbol.get.caseFields.length
       if (air == 0) println(s"[WARNING] Arity of product column ${Format.TypeOf[field]} type ${Format.TypeOf[tpe]} was 0. This is not valid.")
       // Get the field class as an actual string, on the mirror itself it's stored as a type
       val fieldValue = Type.of[field].constValue
       // In certain cases we want to lookup a column not by it's index but by it's name. Most database APIs can do this in general so we add a secondary mechanism
        // this is especially needed if we are using a sum type since in that case, the set of existing fields may actually have fewer things in it then the number of
        // database columns (i.e. since we're using table-per-class which means the table contains all the columns from all the co-product types)
        // if we have a 'column resolver' then we are trying to get the index by name instead by the number. So we lookup if a column resolver exists and use it.
       val resolvedIndex = resolveIndexOrFallback[ResultRow](index, resultRow, fieldValue)
       (Type.of[tpe], summonAndDecode[ResultRow, tpe](resolvedIndex, resultRow)) :: flatten[ResultRow, fields, types]('{$index + ${Expr(air)}}, resultRow)(Type.of[fields], Type.of[types])

      case ('[field *: fields], '[tpe *: types]) =>
        val fieldValue = Type.of[field].constValue
        val resolvedIndex = resolveIndexOrFallback[ResultRow](index, resultRow, fieldValue)
        (Type.of[tpe], summonAndDecode[ResultRow, tpe](resolvedIndex, resultRow)) :: flatten[ResultRow, fields, types]('{$index + 1}, resultRow)(Type.of[fields], Type.of[types])

      case (_, '[EmptyTuple]) => Nil

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)
    }
  }

  def selectMatchingElementAndDecode[Types: Type, ResultRow: Type, T: Type](rawIndex: Expr[Int], resultRow: Expr[ResultRow], rowTypeClassTag: Expr[ClassTag[_]])(typesTup: Type[Types])(using Quotes): Expr[T] =
    import quotes.reflect._
    typesTup match
      case ('[tpe *: types]) =>
        val possibleElementClass =
          Expr.summon[ClassTag[tpe]] match
            case Some(cls) => '{ $cls.runtimeClass }
            case None => report.throwError(s"Cannot summon a ClassTag for the type ${Format.TypeOf[tpe]}")

        val rowTypeClass = '{ $rowTypeClassTag.runtimeClass }
        '{
          // if the type of the coproduct matches the type given by the row-typer, decode from this type (i.e. continue to summonAndDecode)
          // (note that it will summon the column resolver in summonAndDecode for this element instead of using the raw index)
          if ($rowTypeClass.isAssignableFrom($possibleElementClass)) {
            ${ summonAndDecode[ResultRow, tpe](rawIndex, resultRow) }.asInstanceOf[T]
          } else {
            // Otherwise continue to recurse over the remaining types int he coproduct
            ${ selectMatchingElementAndDecode[types, ResultRow, T](rawIndex, resultRow, rowTypeClassTag)(Type.of[types]) }
          }
        }

      case ('[EmptyTuple]) =>
        // Note even when a type in the coproduct matches the rowTypeClassTag, we will still get into this clause
        // because the inlining has to explore every possibility. Therefore we return a runtime error here.
        val msg = s"Cannot resolve coproduct type for '${Format.TypeOf[T]}'"
        '{ throw new IllegalArgumentException(${Expr(msg)}) }



  def decode[T: Type, ResultRow: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using Quotes): Expr[T] = {
    import quotes.reflect._
    // if there is a decoder for the term, just return the term
    Expr.summon[Mirror.Of[T]] match
      case Some(ev) =>
        // Otherwise, recursively summon fields
        ev match {
          case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            // do not treat optional objects as coproduts, a Specific (i.e. EncodingType.Specific) Option-decoder
            // is defined in the EncodingDsl
            if (TypeRepr.of[T] <:< TypeRepr.of[Option[Any]])
              // Try to summon a specific optional from the context, this may not exist since
              // some optionDecoder implementations themselves rely on the context-speicific Decoder[T] which is actually
              // GenericDecoder[ResultRow, T, DecodingType.Specific] since they are Specific, they cannot surround Product types.
              Expr.summon[GenericDecoder[ResultRow, T, DecodingType.Specific]] match
                // TODO Summoning twice in this case (both here and in summonAndDecode, can optimize that)
                case Some(_) => summonAndDecode[ResultRow, T](index, resultRow)
                case None =>
                  // TODO warn about how it can't be "Some[T]" or None for a row-type here?
                  Type.of[T] match
                    case '[Option[t]] =>
                      val decoded = summonAndDecode[ResultRow, t](index, resultRow)
                      '{ Option($decoded) }.asExprOf[T]
            
            else
              // First make sure there is a column resolver, otherwise we can't look up fields by name which 
              // means we can't get specific fields which means we can't decode co-products
              // Technically this should be an error but if I make it into one, the user will have zero feedback as to what is going on and
              // the output will be "Decoder could not be summoned during query execution". At least in this situation
              // the user actually has actionable information on how to resolve the problem.
              Expr.summon[GenericColumnResolver[ResultRow]] match
                case None => 
                  report.warning(
                    s"Need column resolver for in order to be able to decode a coproduct but none exists for ${Format.TypeOf[T]} (row type: ${Format.TypeOf[ResultRow]}). " +
                    s"\nHave you extended the a MirrorContext and made sure to `import ctx.{given, _}`." +
                    s"\nOtherwise a failure will occur with the encoder at runtime")
                  val msg = Expr(s"Cannot Column Resolver does not exist for ${Format.TypeOf[ResultRow]}")
                  '{ throw new IllegalArgumentException($msg) }
                case _ =>
                  // Then summon a 'row typer' which will get us the ClassTag of the actual type (from the list of coproduct types) that we are supposed to decode into
                  Expr.summon[GenericRowTyper[ResultRow, T]] match
                    case Some(rowTyper) => 
                      val rowTypeClassTag = '{ $rowTyper($resultRow) }
                      // then go through the elementTypes and match the one that the rowClass refers to. Then decode it (i.e. recurse on the GenericDecoder with it)
                      selectMatchingElementAndDecode[elementTypes, ResultRow, T](index, resultRow, rowTypeClassTag)(Type.of[elementTypes])      
                    case None => 
                      // Technically this should be an error but if I make it into one, the user will have zero feedback as to what is going on and
                      // the output will be "Decoder could not be summoned during query execution". At least in this situation
                      // the user actually has actionable information on how to resolve the problem.
                      report.warning(s"Need a RowTyper for ${Format.TypeOf[T]}. Have you implemented a RowTyper for it? Otherwise the decoder will fail at runtime if this type is encountered")
                      val msg = Expr(s"Cannot summon RowTyper for type: ${Format.TypeOf[T]}")
                      '{ throw new IllegalArgumentException($msg) }
        
          case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes }} =>
            val children = flatten(index, resultRow)(Type.of[elementLabels], Type.of[elementTypes])
            val types = children.map(_._1)
            val terms = children.map(_._2)
            // Get the constructor
            val tpe = TypeRepr.of[T]
            val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor
            //println(s"========== ParamSymss: ${constructor.paramSymss.map(_.map(_.flags.show))} ================")
            //println(s"========== ParamSymss: ${constructor.paramSymss.map(_.map(_.isTypeParam))} ================")
            //println(s"========== ParamSymss: ${tpe.classSymbol.get.flags.show} ================")
            

            // If we are a tuple, we can easily construct it
            if (tpe <:< TypeRepr.of[Tuple]) {
              val construct =
                Apply(
                  TypeApply(
                    Select(New(TypeTree.of[T]), constructor),
                    types.map { tpe =>
                      tpe match
                        case '[tt] => TypeTree.of[tt]
                    }
                  ),
                  terms.map(_.asTerm)
                )
              //println(s"=========== Create from Tuple Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
              construct.asExprOf[T]
            // If we are a case class with no generic parameters, we can easily construct it
            } else if (tpe.classSymbol.exists(_.flags.is(Flags.Case)) && !constructor.paramSymss.exists(_.exists(_.isTypeParam))) {
              val construct =
                Apply(
                  Select(New(TypeTree.of[T]), constructor),
                  terms.map(_.asTerm)
                )
              //println(s"=========== Create from CaseClass Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
              construct.asExprOf[T]
            // Otherwise, we have to use the mirror itself for construction which is more spliced code
            } else {
              //println(s"=========== Create from Mirror ${Format.Expr(m)} ===========")
              '{ $m.fromProduct(${Expr.ofTupleFromSeq(terms)}) }.asExprOf[T]
            }
            

          case _ => report.throwError("Tuple decoder could not be summoned")
        }

      case _ =>
        report.throwError("Tuple decoder could not be summoned")
  }

  def summon[T: Type, ResultRow: Type](using quotes: Quotes): Expr[GenericDecoder[ResultRow, T, DecodingType.Generic]] =
    import quotes.reflect._
    '{
      new GenericDecoder[ResultRow, T, DecodingType.Generic] {
        def apply(index: Int, resultRow: ResultRow) = ${GenericDecoder.decode[T, ResultRow]('index, 'resultRow)}
      }
    }
}