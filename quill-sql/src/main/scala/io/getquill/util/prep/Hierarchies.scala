package io.getquill.util.prep

case class Inst(instVal: String)

object Mod {
  // val inst = Inst("instValValue")
  // def inst

  def modDefAp() = "modDefApValue"
  def modDef = "modDefValue"
  val modVal = "modValValue"

  object Foo {
    def fooDefAp() = "fooDefApValue"
    def fooDef = "fooDefValue"
    val fooVal = "fooValValue"

    object Bar {
      def barDefAp() = "barDefApValue"
      def barDef = "barDefValue"
      val barVal = "barValValue"
    }
  }
}
