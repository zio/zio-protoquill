
type MyType = String | List[String]
val l: MyType = List("foo", "bar")

val v = l match {
    case _:String => "string"
    case _:List[String] => "string list"
}

v