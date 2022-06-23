package io.getquill

object testContext extends TestMirrorContextTemplate(MirrorIdiom, Literal) with TestEntities
object testContextSnake extends TestMirrorContextTemplate[MirrorSqlDialect, SnakeCase](MirrorSqlDialect, SnakeCase)
