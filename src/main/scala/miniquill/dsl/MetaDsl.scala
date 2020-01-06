package miniquill.dsl

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}


object MetaDsl {
  

  class ValueComputation(given qctx: QuoteContext) {
    import qctx.tasty._

    //private def caseClassConstructor(t: Type) =
      
  }

  inline def playAround[T](stuff: T): Any = ${ playAroundImpl('stuff) }
  def playAroundImpl[T: Type](stuff: Expr[T])(given qctx: QuoteContext): Expr[Any] = {
    null
  }


}




// TODO If want to have Index by a type, need some way to add to it
trait CoreDsl extends EncodingDsl



trait MetaDsl { this: CoreDsl =>
  trait QueryMeta[T] {
    def expand: Query[T] => Query[_] //Quoted[Query[T] => Query[_]]
    def extract: ResultRow => T
  }
}
