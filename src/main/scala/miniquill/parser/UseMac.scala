package miniquill.parser

object UseMac {
  def main(args: Array[String]):Unit = {
    import miniquill.quoter.QueryDsl._
    MatchMac("foo".like("bar")) //hello
  }
}