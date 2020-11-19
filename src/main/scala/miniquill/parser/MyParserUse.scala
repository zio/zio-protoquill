package miniquill.parser

object MyParserUse {
  def main(args: Array[String]):Unit = {
    inline def tup = ("foo", "bar")
    //PrintMac(tup)
    MatchMac(tup)
  }
}
