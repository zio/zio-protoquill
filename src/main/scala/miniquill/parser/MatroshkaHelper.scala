package miniquill.parser

import io.getquill.ast.{ Ident => Idnt, Constant => Const, Query => Qry, _}
import miniquill.quoter._
import scala.quoted._

import scala.annotation.StaticAnnotation
import scala.deriving._
import io.getquill.Embedable
