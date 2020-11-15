// package scratch

// trait NewType[A] {
//   type Type <: A
//   def apply(a: A): Type
// }

// def newType[A]: NewType[A] =
//   new NewType[A] {
//     type Type = A
//     override def apply(a: A): Type = a
// }

// trait Monoid[T] {
//   def empty: T
//   def combine(x: T, y: T): T
// }

// object NewType1 {

//   val Sum: NewType[Int] = newType[Int]
//   type Sum = Sum.Type
//   implicit val summingMonoid: Monoid[Sum] =
//     new Monoid[Sum] {
//       override def empty: Sum = Sum(0)
//       override def combine(x: Sum, y: Sum): Sum = Sum(x + y)
//     }

//   def main(args: Array[String]): Unit = {
//     val sum = implicitly[Monoid[Sum]].combine(Sum(123), Sum(456))
//     println(sum)
//   }
// }




// trait NewTypeInline[A] {
//   type Type = A
//   inline def apply(inline a: A): Type = a
// }

// def newTypeInline[A]: NewTypeInline[A] = new NewTypeInline[A] {}

// object NewTypeInline {

//   val Wrap: NewTypeInline[Int] = newTypeInline[Int]
//   type Wrap = Wrap.Type

//   val Brap: NewTypeInline[Int] = newTypeInline[Int]
//   type Brap = Wrap.Type

//   def main(args: Array[String]): Unit = {
//     implicit inline def elem: Wrap = Wrap.apply(1)
//     val i = 2
//     printTree(implicitly[Wrap] + i) //hello
//   }
// }


// object NewType2 {

//   trait Schema[T]
//   case class Container[T](t: T)

//   val Sum: NewType[Int] = newType[Int]
//   type Sum = Sum.Type
//   implicit val summingMonoid: Monoid[Sum] =
//     new Monoid[Sum] {
//       override def empty: Sum = Sum(0)
//       override def combine(x: Sum, y: Sum): Sum = Sum(x + y)
//     }

//   def main(args: Array[String]): Unit = {
//     val sum = implicitly[Monoid[Sum]].combine(Sum(123), Sum(456))
//     println(sum)
//   }
// }

// // trait NewTypeHigh[A[_]] {
// //   type Type[K] <: A[K]
// //   def apply[K](a: A[K]): Type[K]
// // }

// // def newTypeHigh[A[_]]: NewType[A[_]] =
// //   new NewType[A[_]] {
// //     type Type[K] = A[K]
// //     override def apply[K](a: A[K]): Type[K] = a
// // }

// // trait Schema[T]
// // case class Container[T](t: T)

// // object Api {
// //   val Schema: NewType[Schema] = newTypeHigh[SchemaMeta]
// //   type Schema[K] = Schema.Type[K]
// //   def defineSchema[T](t: T): Schema[T] = Schema.apply[T](Container[T](t))
// // }

// // object SchemaAndQuotationLotEx {
// //   import Api._
// //   case class Person(name: String, age: Int)
// //   def main(args: Array[String]): Unit = {
// //     implicit val schema = defineSchema(new Person("Joe", 123))
// //     // Later...
// //     val c: Container[Person] = implicitly[Schema[Person]]
// //   }
// // }


// trait NewTypePoly[A] {
//   type Type <: A
//   def apply(a: A): Type
//   def toF[F[_]](fa: F[A]): F[TypeRepr]
//   def fromF[F[_]](fa: F[Type]): F[A]
// }

// def newTypePoly[A]: NewTypePoly[A] =
//   new NewTypePoly[A] {
//     type Type = A
//     override def apply(a: A): Type = a
//     override def toF[F[_]](fa: F[A]): F[Type] = fa
//     override def fromF[F[_]](fa: F[Type]): F[A] = fa
//   }

// // Go from SchemaMeta[T] -> QuotationLot[T]
// // define implicit SchemaMeta[T] for some T
// // from the SchemaMeta macro, return a SchemaMeta[T]
// // then summonFrom { case sm: SchemaMeta[T] => SchemaMeta. }

// object NewTypePoly {
//   val Mult: NewTypePoly[Int] = newTypePoly[Int]
//   type Mult = Mult.Type

//   def main(args: Array[String]):Unit = {
//     //val list1: List[Mult] = List(1, 2, 3).map(Mult(_))
//     val list2: List[Mult] = Mult.toF(List(1, 2, 3))

//   }
// }

