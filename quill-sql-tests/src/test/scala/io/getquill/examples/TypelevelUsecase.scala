package io.getquill.examples

import scala.language.implicitConversions
import io.getquill._

object TypelevelUsecase {

  
  case class Address(street: String, zip: Int, fk: Int) extends Embedded //helloooo
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class User(id: Int, name: String)
  case class UserToRole(userId: Int, roleId: Int)
  case class Role(id: Int, name: String)
  case class RoleToPermission(roleId: Int, permissionId: Int)
  case class Permission(id: Int, name: Int)

  trait Path[From, To] {
    type Out
    inline def get: Out
  }
  
  inline given Path[User, Role] with {
    type Out = Query[(User, Role)]
    inline def get: Query[(User, Role)] =
      for {
        s <- query[User]
        sr <- query[UserToRole].join(sr => sr.userId == s.id)
        r <- query[Role].join(r => r.id == sr.roleId)
      } yield (s, r)
  }
  
  inline given Path[User, Permission] with {
    type Out = Query[(User, Role, Permission)]
    inline def get: Query[(User, Role, Permission)] =
      for {
        s <- query[User]
        so <- query[UserToRole].join(so => so.userId == s.id)
        r <- query[Role].join(r => r.id == so.roleId)
        rp <- query[RoleToPermission].join(rp => rp.roleId == r.id)
        p <- query[Permission].join(p => p.id == rp.roleId)
      } yield (s, r, p)
  }
  
  inline def path[F, T](using path: Path[F, T]): path.Out = path.get
  
  inline def q1 = quote { path[User, Role].filter(so => so._2.name == "Drinker") }

  //inline def q1 = quote { path[User, Permission].filter(urp => urp._2.name == "GuiUser" && urp._1.name == "Joe") }
  //inline def q1 = quote { path[User, Permission].filter { case (u,r,p) => u.name == "GuiUser" && r.name == "Joe" } }

  inline def q2 = quote { path[User, Permission].filter((u,r,p) => u.name == "GuiUser" && r.name == "Joe") }
  println( run(q2).string(true) )

  // Made copies of this query to in order to profile for performance
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )
  // println( run(q2).string(true) )

  def main(args: Array[String]): Unit = {
    println( run(q1) )
    println( run(q2).string(true) )
  }
}
