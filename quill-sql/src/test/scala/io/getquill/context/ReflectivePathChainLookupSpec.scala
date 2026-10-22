package io.getquill.context

import io.getquill.Spec
import io.getquill.context.lookupfixtures._

class ReflectivePathChainLookupSpec extends Spec {

  def lookup(obj: Object, path: String*) = ReflectivePathChainLookup(obj, path.toList).map(_.current)

  "a member of a regular class is not mistaken for an object named <class><member>" in {
    lookup(ModConf, "conf", "Schema") mustEqual Right("x")
  }

  "a nested object is not mistaken for a sibling whose name ends with its name" in {
    lookup(Db, "Tables", "Users", "name") mustEqual Right("users")
  }

  "a failure to load a guessed nested object class falls through to the other lookups" in {
    // Behaves like a case-insensitive file system: loading `Mod$db$` finds `Mod$Db$.class`
    // and fails with a NoClassDefFoundError
    val fixturePackage = "io.getquill.context.lookupfixtures.caseinsensitive."
    val loader = new ClassLoader(getClass.getClassLoader) {
      override def loadClass(name: String, resolve: Boolean): Class[_] =
        getClassLoadingLock(name).synchronized {
          if (name == fixturePackage + "Mod$db$")
            throw new NoClassDefFoundError(s"${name.replace('.', '/')} (wrong name: ${fixturePackage.replace('.', '/')}Mod$$Db$$)")
          else if (name.startsWith(fixturePackage))
            Option(findLoadedClass(name)).getOrElse {
              val bytes = getParent.getResourceAsStream(name.replace('.', '/') + ".class").readAllBytes()
              defineClass(name, bytes, 0, bytes.length)
            }
          else super.loadClass(name, resolve)
        }
    }
    val mod = loader.loadClass(fixturePackage + "Mod$").getField("MODULE$").get(null)
    lookup(mod, "db", "url") mustEqual Right("jdbc:x")
  }
}
