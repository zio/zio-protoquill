package io.getquill.context

import scala.quoted._
import io.getquill.ToString
import io.getquill.util.Load
import io.getquill.metaprog.Extractors
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.util.Either
import io.getquill.util.Format

private[getquill] object ReflectivePathChainLookup:
  sealed trait LookupElement { def cls: Class[_]; def current: Object }
  object LookupElement:
    // For a module class the lookup-object is actually a class. For example
    // for: object Foo { object Bar { ... } } you would do:
    //   val submod: Class[Bar] = Class[Foo].getDeclaredClasses.find(_.name endsWith "Bar$")
    //   submod.getField("MODULE$").get(submod /*Object is passed into here*/)
    //   (note that the Class[---] things are typed above but in reality when dealing with them in the reflection they won't be)
    case class ModuleClass(cls: Class[_]) extends LookupElement { val current: Object = cls }
    // For a regular value reference, we can simply lookup the class from the object
    case class Value(current: Object) extends LookupElement { val cls = current.getClass }
  end LookupElement

  case class LookupPath(element: LookupElement, path: String):
    def cls = element.cls
    def current = element.current

  extension (elem: Option[Object])
    def nullCheck(path: String, cls: Class[_], lookupType: String) =
      elem match
        case Some(null) =>
          println(s"The ${lookupType} ${path} can be looked up from ${cls} but the value is null")
          None
        case other =>
          other

  object Lookup:

    // If it's a method on a regular (i.e. dynamic) class. Try to that up
    object Method:
      def unapply(lookup: LookupPath): Option[LookupElement.Value] =
        lookupFirstMethod(lookup.path)(lookup.cls, lookup.current)("method").map(LookupElement.Value(_))

    // if it's a field on a regular (i.e. dynamic) class. Try to look that up
    object Field:
      def unapply(lookup: LookupPath): Option[LookupElement.Value] =
        lookupFirstMethod(lookup.path)(lookup.cls, lookup.current)("field").map(LookupElement.Value(_))

    // Scala object-in-object e.g. object Foo { object Bar { ... } }. Lookup the `Bar`
    // it will be represented in java as: path.to.Foo$Bar.
    object Submodule:
      def unapply(lookup: LookupPath): Option[LookupElement.ModuleClass] =
        val submod = lookup.cls.getDeclaredClasses.find(c => c.getName.endsWith(lookup.path + "$"))
        submod.orElse {
          // Odd pattern for top level object: object Foo { object Bar }
          // there won't be a Bar in getDeclaredClasses but instead a Bar field on Foo whose value is null
          lookup.cls.getFields.find(_.getName == lookup.path).map(_.getType)
        }.map(LookupElement.ModuleClass(_))

    object HelperObjectField:
      def unapply(lookup: LookupPath): Option[LookupElement.Value] =
        // Get Foo.MODULE$
        val submodOpt: Option[Object] = lookupModuleObject(lookup.current)(lookup.cls)
        // Get Foo.MODULE$.fields. The `Field` unapply can be recycled for this purpose
        submodOpt.map(submod =>
          // I.e. lookup MODULE$.field
          lookupFirstMethod(lookup.path)(lookup.cls, submod)("$MODULE.field").map(LookupElement.Value(_))
        ).flatten

    object HelperObjectMethod:
      def unapply(lookup: LookupPath): Option[LookupElement.Value] =
        // Get Foo.MODULE$
        val submodOpt: Option[Object] = lookupModuleObject(lookup.current)(lookup.cls)
        // Get Foo.MODULE$.methods. The `Method` unapply can be recycled for this purpose
        submodOpt.map(submod =>
          // I.e. lookup MODULE$.method
          lookupFirstMethod(lookup.path)(lookup.cls, submod)("$MODULE.method").map(LookupElement.Value(_))
        ).flatten

    // Lookup object Foo { ... } element MODULE$ which is the singleton instance in the Java representation
    def lookupModuleObject(obj: Object)(cls: Class[_] = obj.getClass): Option[Object] =
      cls.getFields.find(_.getName == "MODULE$").map(m => Try(m.get(obj)).toOption).flatten

    def lookupFirstMethod(path: String)(cls: Class[_], instance: Object)(label: String) =
      val methodOpt = cls.getMethods.find(m => m.getName == path && m.getParameterCount == 0)
      methodOpt.map(m => Try(m.invoke(instance)).toOption).flatten.nullCheck(path, cls, label)

    def lookupFirstField(path: String)(cls: Class[_], instance: Object)(label: String) =
      val fieldOpt = cls.getFields.find(_.getName == path)
      fieldOpt.map(f => Try(f.get(instance)).toOption).flatten.nullCheck(path, cls, label)

  end Lookup

  import java.lang.reflect.{Method, Field}
  def singleLookup(elem: LookupElement, path: String): Option[LookupElement] =
    LookupPath(elem, path) match
      case Lookup.Submodule(elem)          => Some(elem)
      case Lookup.Method(elem)             => Some(elem)
      case Lookup.Field(elem)              => Some(elem)
      case Lookup.HelperObjectMethod(elem) => Some(elem)
      case Lookup.HelperObjectField(elem)  => Some(elem)
      case _                               => None

  def chainLookup(element: LookupElement, paths: List[String])(pathsSeen: List[String] = List()): Either[String, LookupElement] =
    import StringOps._
    paths match
      case Nil => Right(element)
      case head :: tail =>
        val nextElementOpt = singleLookup(element, head)
        nextElementOpt match
          case Some(nextElement) =>
            chainLookup(nextElement, tail)(pathsSeen :+ head)
          case None =>
            Left(
              s"Could not look up the path `${pathsSeen.dots}[$head]` from the `${element.cls.getName}` ${element.current}.\n" +
                s"Remaining path: ${paths.mkString(".")}"
            )

  def apply(obj: Object, paths: List[String]) =
    chainLookup(LookupElement.Value(obj), paths)()

  object StringOps:
    extension (strList: List[String])
      def dots =
        if (!strList.isEmpty) strList.mkString("", ".", ".") else ""

end ReflectivePathChainLookup
