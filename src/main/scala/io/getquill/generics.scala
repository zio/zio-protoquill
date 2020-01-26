package io.getquill

import io.getquill.derived.Expander

implicit inline def autoExpander[T]: Expander[T] = Expander.derived