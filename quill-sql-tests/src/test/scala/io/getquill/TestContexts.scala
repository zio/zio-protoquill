package io.getquill

object testContext extends TestMirrorContextTemplate(MirrorIdiom, Literal)
object testContextSnake extends TestMirrorContextTemplate[MirrorSqlDialect, SnakeCase](MirrorSqlDialect, SnakeCase)
