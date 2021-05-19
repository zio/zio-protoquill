package io.getquill.util

object UseSummonMac {
  def main(args: Array[String]): Unit = {
    given s: SingleGenie = SingleGenie
    SummonMac()
  }
}