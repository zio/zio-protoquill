package io.getquill.util

// TODO Move to the testing code
object UseSummonMac {
  def main(args: Array[String]): Unit = {
    given s: SingleGenie = SingleGenie
    SummonMac()
  }
}