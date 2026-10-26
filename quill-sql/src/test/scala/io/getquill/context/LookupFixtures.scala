package io.getquill.context.lookupfixtures {

  // A regular class whose name plus a member name is also the name of an unrelated object
  class Conf { def Schema = "x" }
  object ConfSchema
  object ModConf { val conf = new Conf }

  // Sibling objects where one name is a suffix of the other
  object Db {
    object Tables {
      object ActiveUsers { val name = "active_users" }
      object Users { val name = "users" }
    }
  }
}

package io.getquill.context.lookupfixtures.caseinsensitive {

  // `Mod$Db$` exists, so looking up `Mod$db$` on a case-insensitive file system finds the wrong class file
  object Mod {
    case class Db(url: String)
    val db = Db("jdbc:x")
  }
}
