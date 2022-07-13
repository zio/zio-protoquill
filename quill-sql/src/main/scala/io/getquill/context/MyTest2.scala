// package dao.quill

// import io.getquill.{MysqlEscape, SqlMirrorContext}
// import io.getquill.*
// import io.getquill.extras.*
// import io.getquill.{query as qquery, *}

// import java.sql.Timestamp

// object QuillError {
//   private object ctx extends SqlMirrorContext(PostgresDialect, Literal)
//   case class Person(name: String, age: Int)
//   import ctx._
//   // Note, this gets fixed if we change users to an inline def
//   val v = quote { query[Person] }

//   def main(args: Array[String]): Unit =
//     val joe = Person("Joe", 123)
//     val result = ctx.run(v.filter(u => u.age == lift(55)).updateValue(lift(joe)))
//     // val result = ctx.run(v.filter(u => u.age == 55).updateValue(lift(joe)))
//     println(result)
// }
