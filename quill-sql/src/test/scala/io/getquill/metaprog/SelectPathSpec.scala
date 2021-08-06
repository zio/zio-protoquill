package io.getquill.metaprog

import org.scalatest._
import io.getquill.Spec
import io.getquill.context.ReflectivePathChainLookup
import io.getquill.context.ReflectivePathChainLookup.LookupElement
import io.getquill.util.prep.Mod

class SelectPath extends Spec {
  "Basic ReflectiveLookup should select correct path from" - {
    "Mod.modVal" in {
      ReflectivePathChainLookup.apply(Mod, List("modVal")) mustEqual
        Right(LookupElement.Value("modValValue"))
    }
    "Mod.modDef" in {
      ReflectivePathChainLookup.apply(Mod, List("modDef")) mustEqual
        Right(LookupElement.Value("modDefValue"))
    }
    "Mod.modDefAp" in {
      ReflectivePathChainLookup.apply(Mod, List("modDefAp")) mustEqual
        Right(LookupElement.Value("modDefApValue"))
    }

    "Mod.Foo.fooVal" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooVal")) mustEqual
        Right(LookupElement.Value("fooValValue"))
    }
    "Mod.Foo.fooDef" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooDef")) mustEqual
        Right(LookupElement.Value("fooDefValue"))
    }
    "Mod.Foo.fooDefAp" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooDefAp")) mustEqual
        Right(LookupElement.Value("fooDefApValue"))
    }

    "Mod.Foo.Bar.barVal" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barVal")) mustEqual
        Right(LookupElement.Value("barValValue"))
    }
    "Mod.Foo.Bar.barDef" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barDef")) mustEqual
        Right(LookupElement.Value("barDefValue"))
    }
    "Mod.Foo.Bar.barDefAp()" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barDefAp")) mustEqual
        Right(LookupElement.Value("barDefApValue"))
    }
  }

}