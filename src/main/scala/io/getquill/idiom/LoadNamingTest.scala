package io.getquill.idiom

import io.getquill._

@main def testReflectLoadTypes() = {
  val comp = NamingStrategy(SnakeCase, UpperCase)
  LoadNaming.mac[Literal](Literal)
}

@main def testReflectLoadTypesMulti() = {
  val comp = NamingStrategy(SnakeCase, UpperCase)
  LoadNaming.mac(comp)
}

@main def testLoadNaming() = {
  println( macLoadNamingStrategy[Literal](Literal) )
}

@main def testLoadNamingInferred() = {
  println( macLoadNamingStrategy(Literal) )
}

@main def testLoadNamingMulti() = {
  val comp = NamingStrategy(SnakeCase, UpperCase)
  println( macLoadNamingStrategy(comp) )
}
