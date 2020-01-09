package miniquill.dsl

import miniquill.quoter._
import scala.reflect.ClassTag
import scala.quoted._
import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom}


// trait Expander[T] {
//   def expand: Ast
// }

// object Expander {
  

  

//   inline def derived[T](given m: Mirror.Of[T]): MetaDsl


// }




// TODO If want to have Index by a type, need some way to add to it
trait CoreDsl extends EncodingDsl



trait MetaDsl { this: CoreDsl =>
  trait QueryMeta[T] {
    def expand: Query[T] => Query[_] //Quoted[Query[T] => Query[_]]
    def extract: ResultRow => T
  }
}
