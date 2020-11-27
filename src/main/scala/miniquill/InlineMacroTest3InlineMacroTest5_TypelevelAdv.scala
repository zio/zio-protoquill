package miniquill

import simple.SimpleMacro._
import scala.language.implicitConversions
import miniquill.quoter.Dsl._
import miniquill.quoter._

object InlineMacroTest5_TypelevelAdv {

  import io.getquill._
  case class Address(street: String, zip: Int, fk: Int) extends Embedded //helloooo
  given Embedable[Address] //hello
  case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)
  val ctx = new MirrorContext(MirrorSqlDialect, Literal)
  import ctx._

  case class User(id: Int, name: String)
  case class UserToRole(userId: Int, roleId: Int)
  case class Role(id: Int, name: String)
  case class RoleToPermission(roleId: Int, permissionId: Int)
  case class Permission(id: Int, name: String, venueId: Int)
  case class Venue(id: Int, name: String)

  trait Path[From, To]:
    type Out
    inline def get: Out

  
  class PathFromUserToRole extends Path[User, Role]:
    type Out = Query[(User, Role)]
    inline def get: Query[(User, Role)] =
      for {
        s <- query[User]
        sr <- query[UserToRole].join(sr => sr.userId == s.id)
        r <- query[Role].join(r => r.id == sr.roleId)
      } yield (s, r)
  
  class PathFromUserToPermission extends Path[User, Permission]:
    type Out = Query[(User, Role, Permission)]
    inline def get: Query[(User, Role, Permission)] =
      for {
        u <- query[User] if (u.name == "Joe")
        ur <- query[UserToRole].join(so => so.userId == u.id)
        r <- query[Role].join(r => r.id == ur.roleId) if (r.name == "Drinker")
        rp <- query[RoleToPermission].join(rp => rp.roleId == r.id)
        p <- query[Permission].join(p => p.id == rp.roleId)
        v <- query[Venue].join(v => v.id == p.venueId) if (v.name == "Divebar")
      } yield (u, r, p)
  
  class PathFromUserToVenue extends Path[User, Venue]:
    type Out = Query[(User, Role, Permission, Venue)]
    inline def get: Query[(User, Role, Permission, Venue)] =
      for {
        u <- query[User]
        ur <- query[UserToRole].join(so => so.userId == u.id)
        r <- query[Role].join(r => r.id == ur.roleId)
        rp <- query[RoleToPermission].join(rp => rp.roleId == r.id)
        p <- query[Permission].join(p => p.id == rp.roleId)
        v <- query[Venue].join(v => v.id == p.venueId)
      } yield (u, r, p, v)

  inline given pathFromUserToRole as PathFromUserToRole = new PathFromUserToRole
  inline given pathFromUserToPermission as PathFromUserToPermission = new PathFromUserToPermission
  inline given pathFromUserToVenue as PathFromUserToVenue = new PathFromUserToVenue

  inline def path[F, T](using inline path: Path[F, T]): path.Out = path.get
  
  // inline def q1 = quote { path[User, Role].filter(so => so._1.symbol == "MSFT") }
  // println( run(q1) )

  // .filter(urp => urp._2.name == "Drinker" && urp._1.name == "Joe")
  //inline def q1 = quote { path[User, Permission] }

// (t => t._1.name == "Joe" && t._2.name == "Drinker" && t._3.name == "Drink" && t._4.name == "Divebar")

  inline def q = 
    quote {
      path[User,Venue].filter(t => t._1.name == "Joe" && t._2.name == "Drinker" && t._3.name == "Drink" && t._4.name == "Divebar")
      // path[User,Permission].filter { case (u, r, p, v) =>
      //   u.name == "Joe" && r.name == "Drinker" && p.name == "Drink" && v.name == "Divebar"
      // }
      // path[User,Permission].filter((u, r, p, v) =>
      //   u.name == "Joe" && r.name == "Drinker" && p.name == "Drink" && v.name == "Divebar"
      // )
    }
 //hgello

  //path[User,Permission, Venue]: Query[(User, Role, Permission, Venue)]
  println( run(q).string(true) )


      //path[User, Role].filter(t => t._1.name == "Joe")





  def main(args: Array[String]): Unit = { //hellooooooooooooooooooooooo


  }
}
