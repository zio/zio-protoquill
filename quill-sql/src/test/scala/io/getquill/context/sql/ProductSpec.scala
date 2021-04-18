// package io.getquill.context.sql

// import io.getquill.Spec
// import io.getquill.Query
// import io.getquill.context.Context
// 

// case class Id(value: Long) extends AnyVal

// trait ProductSpec extends Spec {

//   val context: Context[_, _]

//   import context._

//   case class Product(id: Long, description: String, sku: Long)

//   val product = quote {
//     query[Product]
//   }

//   val productInsert = quote {
//     (p: Product) => query[Product].insert(p).returningGenerated(_.id)
//   }

//   val productInsertBatch = quote {
//     (b: Query[Product]) => b.foreach(p => productInsert.apply(p))
//   }

//   def productById = quote {
//     (id: Long) => product.filter(_.id == id)
//   }

//   val productEntries = List(
//     Product(0L, "Notebook", 1001L),
//     Product(0L, "Soap", 1002L),
//     Product(0L, "Pencil", 1003L)
//   )

//   // returningGenerated/returning not implemented yet
//   //val productSingleInsert = quote {
//   //  product.insert(_.id -> 0, _.description -> "Window", _.sku -> 1004L).returningGenerated(_.id)
//   //}
// }
