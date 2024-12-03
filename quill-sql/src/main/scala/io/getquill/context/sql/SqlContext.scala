package io.getquill.context.sql

import java.time.LocalDate
import io.getquill.idiom.Idiom as BaseIdiom

import java.util.{Date, UUID}
import io.getquill.context.Context
import io.getquill.NamingStrategy
import io.getquill.generic.DatabaseVerbs

/**
 * Useful for defining abstract contexts e.g. in tests and multi-database patterns
 *
 * TODO some examples
 */
trait SqlContext[+Idiom <: BaseIdiom, +Naming <: NamingStrategy]
    extends Context[Idiom, Naming]
    with DatabaseVerbs {
}
