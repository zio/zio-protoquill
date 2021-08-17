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
    "Mod.modAp" in {
      ReflectivePathChainLookup.apply(Mod, List("modAp")) mustEqual
        Right(LookupElement.Value("modApValue"))
    }

    "Mod.Foo.fooVal" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooVal")) mustEqual
        Right(LookupElement.Value("fooValValue"))
    }
    "Mod.Foo.fooDef" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooDef")) mustEqual
        Right(LookupElement.Value("fooDefValue"))
    }
    "Mod.Foo.fooAp" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "fooAp")) mustEqual
        Right(LookupElement.Value("fooApValue"))
    }

    "Mod.Foo.Bar.barVal" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barVal")) mustEqual
        Right(LookupElement.Value("barValValue"))
    }
    "Mod.Foo.Bar.barDef" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barDef")) mustEqual
        Right(LookupElement.Value("barDefValue"))
    }
    "Mod.Foo.Bar.barAp()" in {
      ReflectivePathChainLookup.apply(Mod, List("Foo", "Bar", "barAp")) mustEqual
        Right(LookupElement.Value("barApValue"))
    }
  }

}