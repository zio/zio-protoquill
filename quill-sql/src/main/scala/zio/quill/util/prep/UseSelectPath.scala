// package zio.quill.util.prep

// import scala.quoted._
// import zio.quill.context.StaticSpliceMacro.SelectPath

// /** Macro that directly uses the SelectPath for testing purposes */
// private[quill] object UseSelectPath:
//   inline def apply[T](value: T): Option[(String, List[String])] = ${ applyImpl('value) }

//   def applyImpl[T: Type](value: Expr[T])(using Quotes): Expr[Option[(String, List[String])]] =
//     import quotes.reflect._

//     SelectPath()

// end UseSelectPath
