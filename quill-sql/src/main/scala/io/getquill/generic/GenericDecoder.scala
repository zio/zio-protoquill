package io.getquill.generic

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, constValue, summonFrom, summonInline}
import io.getquill.metaprog.TypeExtensions._
import io.getquill.util.Format
import scala.annotation.tailrec
import io.getquill.generic.GenericDecoder.FlattenData

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

object Summon:
  def nullChecker[ResultRow: Type, Session: Type](index: Expr[Int], resultRow: Expr[ResultRow])(using Quotes): Expr[Boolean] =
    import quotes.reflect._
    Expr.summon[GenericNullChecker[ResultRow, Session]] match
      case Some(nullChecker) => '{ $nullChecker($index, $resultRow) }
      case None              =>
        // TODO Maybe check the session type and based on what it is, say "Cannot summon a JDBC null-checker..."
        report.throwError(s"Cannot find a null-checker for the session type ${Format.TypeOf[Session]} (whose result-row type is: ${Format.TypeOf[ResultRow]})")

  def decoder[ResultRow: Type, Session: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): Option[Expr[T]] =
    import quotes.reflect.{Term => QTerm, _}
    // Try to summon a specific decoder, if it's not there, summon a generic one
    Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Specific]] match
      case Some(decoder) =>
        Some('{ $decoder($index, $resultRow, $session) })
      case None =>
        None

  def decoderOrFail[ResultRow: Type, Session: Type, T: Type](index: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): Expr[T] =
    import quotes.reflect._
    decoder[ResultRow, Session, T](index, resultRow, session) match
      case Some(value) => value
      case None =>
        println(s"WARNING Could not summon a decoder for the type: ${io.getquill.util.Format.TypeOf[T]}")
        report.throwError(s"Cannot find decoder for the type: ${Format.TypeOf[T]}")
end Summon

object GenericDecoder {

  def tryResolveIndex[ResultRow: Type](originalIndex: Expr[Int], resultRow: Expr[ResultRow], fieldName: String)(using Quotes) =
    import quotes.reflect._
    Expr.summon[GenericColumnResolver[ResultRow]] match
      case Some(resolver) => Some('{ $resolver($resultRow, ${ Expr(fieldName) }) })
      case None           => None

  // decodedExpr will either be the decoder for a primitive column e.g. for `name` in Person(name: "Joe", age: 123)
  // it will be Decoder[String](1 /*column-index*/) given that our row-data is Row("Joe", 123).
  // Alternatively, if the element we are traversing in `flatten` is a product it will be:
  // the Person-constructor i.e. `new Person("Joe", 123)`
  case class FlattenData(tpe: Type[_], decodedExpr: Expr[_], nullCheckerExpr: Expr[Boolean], index: Int)

  @tailrec
  def flatten[ResultRow: Type, Session: Type, Fields: Type, Types: Type](
      index: Int,
      baseIndex: Expr[Int],
      resultRow: Expr[ResultRow],
      session: Expr[Session]
  )(fieldsTup: Type[Fields], typesTup: Type[Types], accum: List[FlattenData] = List())(using Quotes): List[FlattenData] =
    import quotes.reflect.{Term => QTerm, _}

    (fieldsTup, typesTup) match {
      // Check if the field has a specific user-defined encoder for it e.g. Person(name: String) (i.e. since there is a String encoder)
      // or if it is an embedded entity e.g. Person(name: Name, age: Int) where Name is `case class Name(first: String, last: String)`.
      // TODO summoning `GenericDecoder[ResultRow, T, Session, DecodingType.Specific]` here twice, once in the if statement,
      // then later in summonAndDecode. This can potentially be improved i.e. it can be summoned just once and reused.
      case ('[field *: fields], '[tpe *: types]) if Expr.summon[GenericDecoder[ResultRow, Session, tpe, DecodingType.Specific]].isEmpty =>
        // Get the field class as an actual string, on the mirror itself it's stored as a type
        val fieldValue = Type.of[field].constValue
        val result = decode[tpe, ResultRow, Session](index, baseIndex, resultRow, session)
        // Say we are on Person(id(c1): Int, name: Name(first(c2): String, last(c3): String), age(c4): Int)
        // if we are on the at the last `age` field the last recursion of the `name` field should have bumped our index 3
        val nextIndex = result.index + 1
        flatten[ResultRow, Session, fields, types](nextIndex, baseIndex, resultRow, session)(Type.of[fields], Type.of[types], result +: accum)

      case ('[field *: fields], '[tpe *: types]) =>
        val fieldValue = Type.of[field].constValue
        // If we are in a product that is actually a variant of a co-product, maybe the column needs to be shifted
        // because when decoding we don't care about all the columns. For example,
        //   sealed trait Shape
        //   case class Square(length: Int, width: Int) extends Shape
        //   case class Circle(radius: Int) extends Shape
        //   In that case `tpe` here will be Square/Circle
        // In this case we need to decode ResultRow(shapeType, raidus, length, width) but
        // However, the Square shape will only know about columns List(length, width)
        // the Circle shape will only know about columns: List(radius)
        // The column resolver looks at the expected columns and figures out what the real index is supposed to be.
        // For example if the index is 0, the ColumnResolver will look at Square and resolve that index to the
        // `length` column whose actual index (of ResultRow) as seen above is 3.
        val possiblyShiftedIndex = tryResolveIndex[ResultRow]('{ $baseIndex + ${ Expr(index) } }, resultRow, fieldValue)
        val result = decode[tpe, ResultRow, Session](index, baseIndex, resultRow, session, possiblyShiftedIndex)
        val nextIndex = index + 1
        flatten[ResultRow, Session, fields, types](nextIndex, baseIndex, resultRow, session)(Type.of[fields], Type.of[types], result +: accum)

      case (_, '[EmptyTuple]) => accum

      case _ => report.throwError("Cannot Derive Product during Type Flattening of Expression:\n" + typesTup)
    }
  end flatten

  def decodeOptional[T: Type, ResultRow: Type, Session: Type](index: Int, baseIndex: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): FlattenData =
    import quotes.reflect._
    // Try to summon a specific optional from the context, this may not exist since
    // some optionDecoder implementations themselves rely on the context-speicific Decoder[T] which is actually
    // GenericDecoder[ResultRow, T, Session, DecodingType.Specific] since they are Specific, they cannot surround Product types.
    Expr.summon[GenericDecoder[ResultRow, Session, T, DecodingType.Specific]] match

      // In the case that this is a leaf node
      case Some(_) =>
        val decoder = Summon.decoderOrFail[ResultRow, Session, Option[T]]('{ $baseIndex + ${ Expr(index) } }, resultRow, session)
        val nullChecker = Summon.nullChecker[ResultRow, Session]('{ $baseIndex + ${ Expr(index) } }, resultRow)
        FlattenData(Type.of[Option[T]], decoder, '{ !${ nullChecker } }, index)

      // This is the cases where we have a optional-product element. It could either be a top level
      // element e.g. Option[Row] or a nested element i.e. the Option[Name] in Person(name: Option[Name], age: Int)
      case None =>
        val FlattenData(_, construct, nullCheck, lastIndex) = decode[T, ResultRow, Session](index, baseIndex, resultRow, session)
        val constructOrNone = '{ if (${ nullCheck }) Some[T](${ construct.asExprOf[T] }) else None }
        FlattenData(Type.of[Option[T]], constructOrNone, nullCheck, lastIndex)
  end decodeOptional

  def decodeProduct[T: Type](flattenData: List[FlattenData], m: Expr[Mirror.ProductOf[T]])(using Quotes) =
    import quotes.reflect._

    // E.g. for Person("Joe", 123) the types of the decoded columns i.e. List(Type[String], Type[Int])
    // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
    // they will be types of the constructor i.e. List(Type[Name], Type[Int])
    val types = flattenData.map(_.tpe)

    // E.g. for Person("Joe", 123) the decoder(0,row,session), decoder(1,row,session) columns
    // that turn into Decoder[String]("Joe"), Decoder[Int](123) respectively.
    // (or for Option[T] columns: Decoder[Option[String]]("Joe")))
    // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
    // they will be the constructor and/or any other field-decoders:
    // List((new Name(Decoder("Joe") || Decoder("Bloggs")), Decoder(123))
    // This is what needs to be fed into the constructor of the outer-entity i.e.
    // new Person((new Name(Decoder("Joe") || Decoder("Bloggs")), Decoder(123))
    val productElements = flattenData.map(_.decodedExpr)
    // actually doing the construction i.e. `new Person(...)`
    val constructed = ConstructDecoded[T](types, productElements, m)

    // E.g. for Person("Joe", 123) the List(q"!nullChecker(0,row)", q"!nullChecker(1,row)") columns
    // that eventually turn into List(!NullChecker("Joe"), !NullChecker(123)) columns.
    // Once you are in a product that has a product inside e.g. Person(name: Name("Joe", "Bloggs"), age: 123)
    // they will be the concatenations of the Or-clauses e.g.
    // List( (NullChecker("Joe") || NullChecker("Bloggs")), NullChecker(123))
    // This is what needs to be the null-checker of the outer entity i.e.
    // if ((NullChecker("Joe") || NullChecker("Bloggs")) || NullChecker(123)) Some(new Name(...)) else None
    val nullChecks = flattenData.map(_._3).reduce((a, b) => '{ $a || $b })

    // Pass the constructor of and the column-check expression upstream. We need to pass
    // both because there could be outer products that need to null check columns here.
    // For example, if you have Option(Person(name: Option(Name("Joe", "Bloggs")), age: 123))
    // which basically means, Option(Person(name: Option(Name(DecodeString(column:1), DecodeString(column:2))), DecodeInt(column:3)))
    // given that the row is Row("Joe", "Bloggs", 123)
    // you could be inside the decoder for `Name` which means you have null-checkers NullCheck(column:1), and NullCheck(column:2)
    // that the outer Person object needs to know about since it can also be None (since something is None only if ALL the columns it has are None)
    // that actual check is done in the decodeOptional method
    FlattenData(Type.of[T], constructed, nullChecks, flattenData.last.index)
  end decodeProduct

  private def isOption[T: Type](using Quotes) =
    import quotes.reflect._
    TypeRepr.of[T] <:< TypeRepr.of[Option[Any]]

  private def isBuiltInType[T: Type](using Quotes) =
    import quotes.reflect._
    isOption[T] || (TypeRepr.of[T] <:< TypeRepr.of[Seq[_]])

  def decode[T: Type, ResultRow: Type, Session: Type](index: Int, baseIndex: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session], overriddenIndex: Option[Expr[Int]] = None)(using Quotes): FlattenData =
    import quotes.reflect._
    // index of a possible decoder element if we need one
    lazy val elementIndex = '{ $baseIndex + ${ Expr(index) } }
    // If the type is optional give it totally separate handling
    if (isOption[T]) {
      Type.of[T] match
        case '[Option[tpe]] =>
          decodeOptional[tpe, ResultRow, Session](index, baseIndex, resultRow, session)
    } else
      // specifically if there is a decoder found, allow optional override of the index via a resolver
      val decoderIndex = overriddenIndex.getOrElse(elementIndex)
      Summon.decoder[ResultRow, Session, T](decoderIndex, resultRow, session) match
        case Some(decoder) =>
          val nullChecker = Summon.nullChecker[ResultRow, Session](decoderIndex, resultRow)
          FlattenData(Type.of[T], decoder, '{ !${ nullChecker } }, index)

        case None =>
          Expr.summon[Mirror.Of[T]] match
            case Some(ev) =>
              // Otherwise, recursively summon fields
              ev match
                case '{ $m: Mirror.SumOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } if (!isBuiltInType[T]) =>
                  // do not treat optional objects as coproducts, a Specific (i.e. EncodingType.Specific) Option-decoder
                  // is defined in the EncodingDsl
                  DecodeSum[T, ResultRow, Session, elementTypes](index, baseIndex, resultRow, session)

                case '{ $m: Mirror.ProductOf[T] { type MirroredElemLabels = elementLabels; type MirroredElemTypes = elementTypes } } =>
                  val children = flatten(index, baseIndex, resultRow, session)(Type.of[elementLabels], Type.of[elementTypes]).reverse
                  decodeProduct[T](children, m)

                case _ => report.throwError(s"Decoder for ${Format.TypeOf[T]} could not be summoned. It has no decoder and is not a recognized Product or Sum type.")
              end match
            case _ =>
              report.throwError(s"No Decoder found for ${Format.TypeOf[T]} and it is not a class representing a group of columns")
          end match
      end match
  end decode

  def summon[T: Type, ResultRow: Type, Session: Type](using quotes: Quotes): Expr[GenericDecoder[ResultRow, Session, T, DecodingType.Generic]] =
    import quotes.reflect._
    '{
      new GenericDecoder[ResultRow, Session, T, DecodingType.Generic] {
        def apply(baseIndex: Int, resultRow: ResultRow, session: Session) = ${ GenericDecoder.decode[T, ResultRow, Session](0, 'baseIndex, 'resultRow, 'session).decodedExpr }.asInstanceOf[T]
      }
    }
}

object DecodeSum:
  def apply[T: Type, ResultRow: Type, Session: Type, ElementTypes: Type](index: Int, baseIndex: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session])(using Quotes): FlattenData =
    import quotes.reflect._
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
            s"\nOtherwise a failure will occur with the encoder at runtime"
        )
        val msg = Expr(s"Cannot summon a Column Resolver. Does not exist for ${Format.TypeOf[ResultRow]}")
        FlattenData(Type.of[T], '{ throw new IllegalArgumentException($msg) }, '{ false }, 0)
      case _ =>
        // Then summon a 'row typer' which will get us the ClassTag of the actual type (from the list of coproduct types) that we are supposed to decode into
        Expr.summon[GenericRowTyper[ResultRow, T]] match
          case Some(rowTyper) =>
            val rowTypeClassTag = '{ $rowTyper($resultRow) }
            // then go through the elementTypes and match the one that the rowClass refers to. Then decode it (i.e. recurse on the GenericDecoder with it)
            selectMatchingElementAndDecode[ElementTypes, ResultRow, Session, T](index, baseIndex, resultRow, session, rowTypeClassTag)(Type.of[ElementTypes])
          case None =>
            // Technically this should be an error but if I make it into one, the user will have zero feedback as to what is going on and
            // the output will be "Decoder could not be summoned during query execution". At least in this situation
            // the user actually has actionable information on how to resolve the problem.
            report.warning(s"Need a RowTyper for ${Format.TypeOf[T]}. Have you implemented a RowTyper for it? Otherwise the decoder will fail at runtime if this type is encountered")
            val msg = Expr(s"Cannot summon RowTyper for type: ${Format.TypeOf[T]}")
            FlattenData(Type.of[T], '{ throw new IllegalArgumentException($msg) }, '{ false }, 0)

  /** Find a type from a coproduct type that matches a given ClassTag, if it matches, summon a decoder for it and decode it */
  def selectMatchingElementAndDecode[Types: Type, ResultRow: Type, Session: Type, T: Type](index: Int, rawIndex: Expr[Int], resultRow: Expr[ResultRow], session: Expr[Session], rowTypeClassTag: Expr[ClassTag[_]])(typesTup: Type[Types])(using
      Quotes
  ): FlattenData =
    import quotes.reflect._
    typesTup match
      case ('[tpe *: types]) =>
        println(s"(Co-Product) Checking if ${Format.TypeOf[tpe]} == ${Format.Expr(rowTypeClassTag)} and should be spliced into index: ${index}")
        val possibleElementClass =
          Expr.summon[ClassTag[tpe]] match
            case Some(cls) => '{ $cls.runtimeClass }
            case None      => report.throwError(s"Cannot summon a ClassTag for the type ${Format.TypeOf[tpe]}")

        // Co-product element may be a product e.g:
        // sealed trait Shape
        // case class Square(length: Int, width: Int) extends Shape
        // case class Circle(radius: Int) extends Shape
        // In that case `tpe` here will be Square/Circle
        val thisElementDecoder = GenericDecoder.decode[tpe, ResultRow, Session](index, rawIndex, resultRow, session).decodedExpr

        val thisElementNullChecker = '{ !${ Summon.nullChecker[ResultRow, Session](rawIndex, resultRow) } }
        // make the recursive call
        val nextData = selectMatchingElementAndDecode[types, ResultRow, Session, T](index + 1, rawIndex, resultRow, session, rowTypeClassTag)(Type.of[types])

        val rowTypeClass = '{ $rowTypeClassTag.runtimeClass }
        val decodedElement =
          '{
            if ($rowTypeClass.isAssignableFrom($possibleElementClass)) {
              ${ thisElementDecoder }
            } else {
              ${ nextData.decodedExpr }
            }
          }

        // if all the possible columns of a coproduct are null the whole thing is null e.g:
        // Person(name: String, age: Int) | Robot(serialNumber: Int) would be represented in the DB as:
        // Row(name, age, serialNumber) so we want Row(null, null, null) i.e.
        // val isNull = !(nullCheck(name) || nullCheck(age) || nullCheck(serialNumber))
        val totalNullCheck = '{ ${ thisElementNullChecker } || ${ nextData.nullCheckerExpr } }
        FlattenData(Type.of[tpe], decodedElement, totalNullCheck, index)

      case ('[EmptyTuple]) =>
        // Note even when a type in the coproduct matches the rowTypeClassTag, we will still get into this clause
        // because the inlining has to explore every possibility. Therefore we return a runtime error here.
        val msg = s"Cannot resolve coproduct type for '${Format.TypeOf[T]}'"
        FlattenData(Type.of[Nothing], '{ throw new IllegalArgumentException(${ Expr(msg) }) }, '{ false }, 0)
end DecodeSum

object ConstructDecoded:
  def apply[T: Type](types: List[Type[_]], terms: List[Expr[_]], m: Expr[Mirror.ProductOf[T]])(using Quotes) = {
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
end ConstructDecoded
