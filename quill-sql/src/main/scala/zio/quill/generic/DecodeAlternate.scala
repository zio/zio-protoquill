package zio.quill.generic

import scala.reflect.ClassTag
import scala.compiletime.{erasedValue, summonFrom, constValue}
import zio.quill.ast.{Tuple => AstTuple, Map => AMap, Query => AQuery, _}
import scala.compiletime.erasedValue
import zio.quill.ast.Visibility.{Hidden, Visible}
import scala.deriving._
import scala.quoted._
import zio.quill.parser.Lifter
import zio.quill.quat.Quat
import zio.quill.ast.{Map => AMap, _}
import zio.quill.metaprog.TypeExtensions
import zio.quill.util.Format

object DecodeAlternate {}
