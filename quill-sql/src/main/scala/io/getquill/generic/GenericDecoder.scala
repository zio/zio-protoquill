package io.getquill.generic

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}
import io.getquill.metaprog.TypeExtensions._
import io.getquill.util.Format
import scala.annotation.tailrec

trait GenericColumnResolver[ResultRow] {
  def apply(resultRow: ResultRow, columnName: String): Int
}

sealed trait DecodingType
object DecodingType {
  sealed trait Generic extends DecodingType
  sealed trait Specific extends DecodingType
}

trait GenericDecoder[ResultRow, Session, T, +DecType <: DecodingType] extends ((Int, ResultRow, Session) => T) {
  def apply(i: Int, rr: ResultRow, s: Session): T
}

trait GenericRowTyper[ResultRow, Co] {
  def apply(rr: ResultRow): ClassTag[_]
}

object GenericDecoder {

  def resolveIndexOrFallback[ResultRow: Type](originalIndex: Expr[Int], resultRow: Expr[ResultRow], fieldName: String)(using Quotes) =
    import quotes.reflect._
    Expr.summon[GenericColumnResolver[ResultRow]] match
      case Some(resolver) => '{ $resolver($resultRow, ${ Expr(fieldName) }) }
      case None           => originalIndex

  def summonNullChecker[ResultRow: Type, Session: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    Expr.summon[GenericNullChecker[ResultRow, Session]] match
      case Some(nullChecker) => '{ $nullChecker($index, $resultRow) }
      case None              =>
        // TODO Maybe check the session type and based on what it is, say "Cannot summon a JDBC null-chercker..."
        report.throwError(s"Cannot find a null-checker for the session type ${Format.TypeOf[Session]} (whose result-row type is: ${Format.TypeOf[ResultRow]})")

  def summonDecoder[ResultRow: Type, Session: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): Expr[T] =
    import quotes.reflect.{Term => QTerm, _}
    // Try to summon a specific decoder, if it's not there, summon a generic one
    Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Specific]] match
      case Some(decoder) =>
        '{ $decoder($index, $resultRow, $session) }
      case None =>
        Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Generic]] match
          case Some(decoder) => '{ $decoder($index, $resultRow, $session) }
          case _ =>
            println(s"WARNING Could not summon a decoder for the type: ${io.getquill.util.Format.TypeOf[T]}")
            report.throwError(s"Cannot find decoder for the type: ${Format.TypeOf[T]}")

  // decodedExpr will either be the decoder for a primitive column e.g. for `name` in Person(name: "Joe", age: 123)
  // it will be Decoder[String](1 /*column-index*/) given that our row-data is Row("Joe", 123).
  // Alternatively, if the element we are traversing in `flatten` is a product it will be:
  // the Person-constructor i.e. `new Person("Joe", 123)`
  case class FlattenData(tpe: Type[_], decodedExpr: Expr[_], nullCheckerExpr: Expr[Boolean])

  @tailrec
  def flatten[ResultRow: Type, Session: Type, Fields: Type, Types: Type](
      index: Expr[Int],
      resultRow: Expr[ResultRow],
      session: Expr[Session]
  )(fieldsTup: Type[Fields], typesTup: Type[Types], accum: List[FlattenData] = List())(using Quotes): List[FlattenData] = {
    import quotes.reflect.{Term => QTerm, _}

    (fieldsTup, typesTup) match {
      // Check if the field has a specific user-defined encoder for it e.g. Person(name: String) (i.e. since there is a String encoder)
      // or if it is an embedded entity e.g. Person(name: Name, age: Int) where Name is `case class Name(first: String, last: String)`.
      // TODO summoning `GenericDecoder[ResultRow, T, Session, DecodingType.Specific]` here twice, once in the if statement,
      // then later in summonAndDecode. This can potentially be improved i.e. it can be summoned just once and reused.
      case ('[field *: fields], '[tpe *: types]) if Expr.summon[GenericDecoder[ResultRow, Session, tpe, DecodingType.Specific]].isEmpty =>
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
        val result = {
          val tpe = Type.of[tpe]
          val decoder = summonDecoder[ResultRow, Session, tpe](resolvedIndex, resultRow, session)
          val nullChecker = summonNullChecker[ResultRow, Session](resolvedIndex, resultRow)
          FlattenData(tpe, decoder, nullChecker)
        }
        flatten[ResultRow, Session, fields, types]('{ $index + ${ Expr(air) } }, resultRow, session)(Type.of[fields], Type.of[types], result +: accum)

      case ('[field *: fields], '[tpe *: types]) =>
        val fieldValue = Type.of[field].constValue
        val resolvedIndex = resolveIndexOrFallback[ResultRow](index, resultRow, fieldValue)
        val result = {
          val tpe = Type.of[tpe]
          val decoder = summonDecoder[ResultRow, Session, tpe](resolvedIndex, resultRow, session)
          val nullChecker = summonNullChecker[ResultRow, Session](resolvedIndex, resultRow)
          FlattenData(tpe, decoder, nullChecker)
        }
        flatten[ResultRow, Session, fields, types]('{ $index + 1 }, resultRow, session)(Type.of[fields], Type.of[types], result +: accum)

      case (_, '[EmptyTuple]) => accum

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)
    }
  }

  /** Find a type from a coproduct type that matches a given ClassTag, if it matches, summon a decoder for it and decode it */
  def selectMatchingElementAndDecode[Types: Type, ResultRow: Type, Session: Type, T: Type](rawIndex: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session], rowTypeClassTag: Expr[ClassTag[_]])(typesTup: Type[Types])(using
      Quotes
  ): Expr[T] =
    import quotes.reflect._
    typesTup match
      case ('[tpe *: types]) =>
        val possibleElementClass =
          Expr.summon[ClassTag[tpe]] match
            case Some(cls) => '{ $cls.runtimeClass }
            case None      => report.throwError(s"Cannot summon a ClassTag for the type ${Format.TypeOf[tpe]}")

        val rowTypeClass = '{ $rowTypeClassTag.runtimeClass }
        '{
          // if the type of the coproduct matches the type given by the row-typer, decode from this type (i.e. continue to summonAndDecode)
          // (note that it will summon the column resolver in summonAndDecode for this element instead of using the raw index)
          if ($rowTypeClass.isAssignableFrom($possibleElementClass)) {
            ${ summonDecoder[ResultRow, Session, tpe](rawIndex, resultRow, session) }.asInstanceOf[T]
          } else {
            // Otherwise continue to recurse over the remaining types int he coproduct
            ${ selectMatchingElementAndDecode[types, ResultRow, Session, T](rawIndex, resultRow, session, rowTypeClassTag)(Type.of[types]) }
          }
        }

      case ('[EmptyTuple]) =>
        // Note even when a type in the coproduct matches the rowTypeClassTag, we will still get into this clause
        // because the inlining has to explore every possibility. Therefore we return a runtime error here.
        val msg = s"Cannot resolve coproduct type for '${Format.TypeOf[T]}'"
        '{ throw new IllegalArgumentException(${ Expr(msg) }) }

  def decodeOptional[T: Type, ResultRow: Type, Session: Type](index: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): FlattenData = {
    import quotes.reflect._
    // Try to summon a specific optional from the context, this may not exist since
    // some optionDecoder implementations themselves rely on the context-speicific Decoder[T] which is actually
    // GenericDecoder[ResultRow, T, Session, DecodingType.Specific] since they are Specific, they cannot surround Product types.
    Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Specific]] match

      // In the case that this is a leaf node
      case Some(_) =>
        val decoder = summonDecoder[ResultRow, Session, T](index, resultRow, session)
        val nullChecker = summonNullChecker[ResultRow, Session](index, resultRow)
        FlattenData(Type.of[T], decoder, nullChecker)

      // This is the cases where we have a optional-product element. It could either be a top level
      // element e.g. Option[Row] or a nested element i.e. the Option[Name] in Person(name: Option[Name], age: Int)
      case None =>
        val FlattenData(_, construct, nullCheck) = decode[T, ResultRow, Session](index, resultRow, session)
        val constructOrNone = '{ if (${ nullCheck }) Some($construct) else None }
        FlattenData(Type.of[Option[T]], constructOrNone, nullCheck)
  }

  def construct[T: Type](types: List[Type[_]], terms: List[Expr[_]], m: Expr[Mirror.ProductOf[T]])(using Quotes) = {
    import quotes.reflect._
    // Get the constructor
    val tpe = TypeRepr.of[T]
    val constructor = TypeRepr.of[T].typeSymbol.primaryConstructor
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
      // println(s"=========== Create from Tuple Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
      construct.asExprOf[T]
      // If we are a case class with no generic parameters, we can easily construct it
    } else if (tpe.classSymbol.exists(_.flags.is(Flags.Case)) && !constructor.paramSymss.exists(_.exists(_.isTypeParam))) {
      val construct =
        Apply(
          Select(New(TypeTree.of[T]), constructor),
          terms.map(_.asTerm)
        )
      // println(s"=========== Create from CaseClass Constructor ${Format.Expr(construct.asExprOf[T])} ===========")
      construct.asExprOf[T]
      // Otherwise, we have to use the mirror itself for construction which is more spliced code
    } else {
      // println(s"=========== Create from Mirror ${Format.Expr(m)} ===========")
      '{ $m.fromProduct(${ Expr.ofTupleFromSeq(terms) }) }.asExprOf[T]
    }
  }

  private def isOption[T: Type](using Quotes) =
    import quotes.reflect._
    TypeRepr.of[T] <:< TypeRepr.of[Option[Any]]

  def decode[T: Type, ResultRow: Type, Session: Type](index: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): FlattenData = {
    import quotes.reflect._
    // if there is a decoder for the term, just return the term
    if (isOption[T]) {
      Type.of[T] match
        case '[Option[tpe]] =>
          decodeOptional[tpe, ResultRow, Session](index, resultRow, session)
    } else
      Expr.summon[Mirror.Of[T]] match
        case Some(ev) =>
          // Otherwise, recursively summon fields
          ev match {
            case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
              // do not treat optional objects as coproduts, a Specific (i.e. EncodingType.Specific) Option-decoder
              // is defined in the EncodingDsl
              report.throwError("Not supported")

            case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
              val children = flatten(index, resultRow, session)(Type.of[elementLabels], Type.of[elementTypes]).reverse

              // E.g. for Person("Joe", 123) the types of the decoded columns i.e. List(Type[String], Type[Int])
              // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
              // they will be types of the constructor i.e. List(Type[Name], Type[Int])
              val types = children.map(_.tpe)

              // E.g. for Person("Joe", 123) the decoder(0,row,session), decoder(1,row,session) columns
              // that turn into Decoder[String]("Joe"), Decoder[Int](123) respectively.
              // (or for Option[T] columns: Decoder[Option[String]]("Joe")))
              // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
              // they will be the constructor and/or any other field-decoders:
              // List((new Name(Decoder("Joe") || Decoder("Bloggs")), Decoder(123))
              // This is what needs to be fed into the constructor of the outer-entity i.e.
              // new Person((new Name(Decoder("Joe") || Decoder("Bloggs")), Decoder(123))
              val productElments = children.map(_._2)
              // actually doing the construction i.e. `new Person(...)`
              val constructed = construct[T](types, productElments, m)

              // E.g. for Person("Joe", 123) the List(q"!nullChecker(0,row)", q"!nullChecker(1,row)") columns
              // that eventually turn into List(!NullChecker("Joe"), !NullChecker(123)) columns.
              // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
              // they will be the concatonations of the Or-clauses e.g.
              // List( (NullChecker("Joe") || NullChecker("Bloggs")), NullChecker(123))
              // This is what needs to be the null-checker of the outer entity i.e.
              // if ((NullChecker("Joe") || NullChecker("Bloggs")) || NullChecker(123)) Some(new Name(...)) else None
              val nullChecks = children.map(_._3).reduce((a, b) => '{ $a || $b })

              // Pass the constructor of and the column-check expression upstream. We need to pass
              // both because there could be outer products that need to null check columns here.
              // For example, if you have Option(Person(name: Option(Name("Joe", "Bloggs")), age: 123))
              // which basically means, Option(Person(name: Option(Name(DecodeString(column:1), DecodeString(column:2))), DecodeInt(column:3)))
              // given that the row is Row("Joe", "Bloggs", 123)
              // you could be inside the decoder for `Name` which means you have null-checkers NullCheck(column:1), and NullCheck(column:2)
              // that the outer Person object needs to know about since it can also be None (since something is None only if ALL the columns it has are None)
              // that actual check is done in the decodeOptional method
              FlattenData(Type.of[T], constructed, nullChecks)

            case _ => report.throwError("Tuple decoder could not be summoned")
          }

        case _ =>
          report.throwError("Tuple decoder could not be summoned")
  }

  def summon[T: Type, ResultRow: Type, Session: Type](using quotes: Quotes): Expr[GenericDecoder[ResultRow, Session, T, DecodingType.Generic]] =
    import quotes.reflect._
    '{
      new GenericDecoder[ResultRow, Session, T, DecodingType.Generic] {
        def apply(index: Int, resultRow: ResultRow, session: Session) = ${ GenericDecoder.decode[T, ResultRow, Session]('index, 'resultRow, 'session).decodedExpr }.asInstanceOf[T]
      }
    }
}
