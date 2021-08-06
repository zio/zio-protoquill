package io.getquill.metaprog

import org.scalatest._
import io.getquill.Spec
import io.getquill.context.ReflectivePathChainLookup
import io.getquill.util.prep.Module1

object SelectPath { //extends Spec {
  // "Basic ReflectiveLookup should select correct path from" in {

  // }

  def main(args: Array[String]): Unit = {
    println( ReflectivePathChainLookup.apply(Module1, List("Foo", "Bar", "barVal")) )

    // .map(_.getName)
    //println(Module.getClass.getFields.toList(0).get(Module))

    //println(Module.getClass.getDeclaredClasses.toList)
    //println(Module.getClass.getField("MODULE$").get(Module).getClass.getDeclaredClasses.toList)




  }
}