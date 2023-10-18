package io.getquill.util.prep

case class Inst(instVal: String)

object Mod {
  // val inst = Inst("instValValue")
  // def inst

  def modAp()   = "modApValue"
  def modDef    = "modDefValue"
  val modVal    = "modValValue"
  val modIntVal = 123

  object Foo {
    def fooAp() = "fooApValue"
    def fooDef  = "fooDefValue"
    val fooVal  = "fooValValue"

    object Bar {
      def barAp() = "barApValue"
      def barDef  = "barDefValue"
      val barVal  = "barValValue"
    }
  }
}
