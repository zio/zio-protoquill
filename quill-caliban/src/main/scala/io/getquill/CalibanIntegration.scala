package io.getquill

import caliban.CalibanError.ExecutionError
import caliban.GraphQL.graphQL
import caliban.introspection.adt.{__InputValue, __Type, __TypeKind}
import caliban.schema.{ArgBuilder, Schema, Step}
import caliban.{InputValue, RootResolver}
import caliban.execution.Field
import caliban.schema.Types
import caliban.Value

case class ProductArgs[T](keyValues: Map[String, String])

object CalibanIntegration {

  def quillColumns(field: Field) = {
    def recurseFetchFields(field: Field): List[Field] =
      if (Types.innerType(field.fieldType).kind == __TypeKind.OBJECT)
        field.fields.flatMap(recurseFetchFields(_))
      else
        List(field)
    field.fields.flatMap(recurseFetchFields(_)).map(_.name)
  }

  def flattenToPairs(key: String, value: InputValue): List[(String, String)] =
    value match {
      // If it contains other keys, continue to get the pairs inside
      // e.g. for `name` in Person(name: Name, age: Int) this would be Name from which we need first:String, last:String
      case InputValue.ObjectValue(fields) => fields.toList.flatMap { case (k, v) => flattenToPairs(k, v) }
      // Need to look at StringValue directly because calling .toInputString on it will give double quoting i.e. "\"value\""
      case Value.StringValue(value) => List((key, value))
      case _                        => List((key, value.toInputString))
    }

  implicit def productArgBuilder[T]: ArgBuilder[ProductArgs[T]] = {
    case InputValue.ObjectValue(fields) =>
      Right(ProductArgs[T](fields.flatMap { case (k, v) => flattenToPairs(k, v).toMap }))
    case other => Left(ExecutionError(s"Can't build a ProductArgs from input $other"))
  }

  implicit def productSchema[T](implicit ev: Schema[Any, T]): Schema[Any, ProductArgs[T]] =
    new Schema[Any, ProductArgs[T]] {

      def makeOptionalRecurse(f: __InputValue): __InputValue = {
        val fieldType = f.`type`()
        val optionalFieldType = fieldType.kind match {
          case __TypeKind.NON_NULL => fieldType.ofType.getOrElse(fieldType)
          case _                   => fieldType
        }
        f.copy(`type` =
          () => optionalFieldType.copy(inputFields = optionalFieldType.inputFields.map(_.map(makeOptionalRecurse)))
        )
      }

      protected[this] def toType(isInput: Boolean, isSubscription: Boolean): __Type =
        __Type(
          __TypeKind.INPUT_OBJECT,
          inputFields = ev
            .toType_(isInput, isSubscription)
            .inputFields
            .map(_.map(f => makeOptionalRecurse(f)))
        )

      def resolve(value: ProductArgs[T]): Step[Any] = Step.NullStep
    }

} // end CalibanIntegration
