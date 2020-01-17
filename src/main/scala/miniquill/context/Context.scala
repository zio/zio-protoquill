package miniquill.context

import miniquill.quoter._
import io.getquill.idiom.Idiom
import io.getquill.NamingStrategy
import io.getquill.context.Context
import scala.quoted._

/*
Implementation of inline macro calls from io.getquill.Context will delegate to here.
This is necessary because locations called by the macros need to be static.
This means for example that it is no longer possible to just have whatever
methods were defined under {prefix} to just be called. Instead a pointer to the
function needs to be passed in order to get access to it's methods.
*/
object Context {

  // TODO There's a lot of ceremony here. Can we put some of this stuff in (i.e. the implicit stuff)
  // via implicit function types? Since all of this stuff needs to be repeated for every flavor
  // of the 'run' function.
  def run[T: Type, Dialect <: Idiom: Type, Naming <: NamingStrategy: Type](
    query: Quoted[T], context: Context[Dialect, Naming])(qctx: QuoteContext): String = {


    // Extract the quotation, check if it is compile-time or run-time
    // if it has any lifted, quotations, it must be run time since otherwise they would be inlined.

    // - If it is runtime, call the traversal function that will recursively inline
    // all the quotations in the AST (define the logic for that in the Quotation method itself)
    
    // - If it is compile-time just pull out the ASt tree

    // Either way need to:
    // - run the expander on the AST to express the full record T
    // - run the 'translate' method in the dialect to get the 'statement' object
    //   -- this will normalize the ast
    // - run the encoders on the row in order to prepare it
    // - call the executeQuery in the context

    // files to bring in MirrorContext
    // MirrorDialect (don't do normalize for now. Will bring it in when creating quill-core-portable)

    query.ast.toString
  }
}