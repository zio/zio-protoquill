package io.getquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import io.getquill.quoter.Dsl._

object TypelevelUsecase_WithPassin {

  import io.getquill._
  case class Address(street: String, zip: Int, fk: Int) extends Embedded //helloooo
  case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class User(id: Int, name: String)
  case class UserToRole(userId: Int, roleId: Int)
  case class Role(id: Int, name: String)
  case class RoleToPermission(roleId: Int, permissionId: Int)
  case class Permission(id: Int, name: Int)

  trait Path[From, To]:
    type Out
    inline def get(inline from: From): Out
  
  inline given Path[User, Role] with
    type Out = Query[(User, Role)]
    inline def get(inline s: User): Query[(User, Role)] =
      for {
        sr <- query[UserToRole].join(sr => sr.userId == s.id)
        r <- query[Role].join(r => r.id == sr.roleId)
      } yield (s, r)
  
  inline given Path[User, Permission] with
    type Out = Query[(User, Role, Permission)]
    inline def get(inline s: User): Query[(User, Role, Permission)] =
      for {
        so <- query[UserToRole].join(so => so.userId == s.id)
        r <- query[Role].join(r => r.id == so.roleId)
        rp <- query[RoleToPermission].join(rp => rp.roleId == r.id)
        p <- query[Permission].join(p => p.id == rp.roleId)
      } yield (s, r, p)

  inline def path[F, T](inline from: F)(using inline path: Path[F, T]): path.Out = path.get(from)
  
  inline def joes = query[User].filter(u => u.name == "Joe")
  // Change to 'symbol' and odd set of explosions happen
  inline def q1 = quote { joes.flatMap(j => path[User, Role](j)).filter(so => so._2.name == "Drinker") }
  

  //inline def q1 = quote { path[User, Permission].filter(urp => urp._2.name == "GuiUser" && urp._1.name == "Drinker") }
  //inline def q1 = quote { path[User, Permission].filter { case (u,r,p) => u.name == "GuiUser" && r.name == "Drinker" } }

  // inline def q2 = quote { joes.flatMap(j => path[User, Permission](j)).filter((u,r,p) => u.name == "GuiUser" && r.name == "Drinker") }
  
  def main(args: Array[String]): Unit = {
    // Need to have queries printing in 'main' or some method that actually gets invoked
    // otherwise compiler seems to not initialize them somehow
    println( run(q1) )
    // println( run(q2).string(true) )
  }
}
