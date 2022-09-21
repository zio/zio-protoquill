package io.getquill.parser

import io.getquill.parser.ParserHelpers.ImplicitClassExtensionPattern
import io.getquill._

class ImplicitClassDetectionSpec extends Spec {
  case class Person(name: String, age: Int)
  implicit class LimitQuery[T](q: Query[T]) {
    inline def limitQuery = quote(sql"$q LIMIT 1".as[Query[T]])
  }

  "should detect implicit class Cls[T].prop" in {
    // Can check that this doesn't compile but can't actually check the message correctness
    // uncomment to see. It should return a detailed warning about why the above construct is invalid
    // like the following:
    //   Error in the expression:
    //     ImplicitClassDetectionSpec.this.LimitQuery[Person](
    //     (EntityQuery.apply[Person]: EntityQuery[Person])
    //   )
    //
    //   Attempted to use an implicit extensions class `TermRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class parser)),class ImplicitClassDetectionSpec)),method LimitQuery)`.
    //   Implicit extensions in Quotations are not supported in ProtoQuill, they can
    //   only be used with dynamic queries. Instead of implicit-classes, use inline
    //   extension methods. For example, instead of doing this:
    //   implicit class TermRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class parser)),class ImplicitClassDetectionSpec)),method LimitQuery)(input: EntityQuery[Person]):
    //     def myMethod = [method content]
    //
    //   Do this:
    //   extension (inline input: EntityQuery[Person])
    //     inline def myMethod = [method content]

    "query[Person].limitQuery" mustNot compile
  }
}
