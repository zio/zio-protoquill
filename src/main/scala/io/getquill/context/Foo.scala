package io.getquill.context

object Foo:

  class Bar(v: String):
    def something(v: String) = Foo.method(v)

  def method(v: String) =
    v.toUpperCase

end Foo