// package miniquill

// import simple.SimpleMacro._
// import scala.language.implicitConversions
// import miniquill.quoter.Dsl._

// object InlineMacroTest5_TypelevelAdv {

//   import io.getquill._
//   case class Address(street: String, zip: Int, fk: Int) extends Embedded //helloooo
//   given Embedable[Address] //hello
//   case class Person(id: Int, name: String, age: Int, addr: Address, middleName: String, lastName: String)
//   val ctx = new MirrorContext(MirrorSqlDialect, Literal)
//   import ctx._

//   case class User(id: Int, name: String)
//   case class UserToRole(userId: Int, roleId: Int)
//   case class Role(id: Int, name: String)
//   case class RoleToPermission(roleId: Int, permissionId: Int)
//   case class Permission(id: Int, name: Int)

//   trait Path[From, To]:
//     type Out
//     inline def get: Out
  
//   class PathFromUserToRole extends Path[User, Role]:
//     type Out = Query[(User, Role)]
//     inline def get: Query[(User, Role)] =
//       for {
//         s <- query[User]
//         sr <- query[UserToRole].join(sr => sr.userId == s.id)
//         r <- query[Role].join(r => r.id == sr.roleId)
//       } yield (s, r)
  
//   class PathFromUserToPermission extends Path[User, Permission]:
//     type Out = Query[(User, Role, Permission)]
//     inline def get: Query[(User, Role, Permission)] =
//       for {
//         s <- query[User]
//         so <- query[UserToRole].join(so => so.userId == s.id)
//         r <- query[Role].join(r => r.id == so.roleId)
//         rp <- query[RoleToPermission].join(rp => rp.roleId == r.id)
//         p <- query[Permission].join(p => p.id == rp.roleId)
//       } yield (s, r, p)
  
//   inline given pathFromUserToRole as PathFromUserToRole = new PathFromUserToRole
//   inline given pathFromUserToPermission as PathFromUserToPermission = new PathFromUserToPermission

//   inline def path[F, T](using inline path: Path[F, T]): path.Out = path.get
  
//   // inline def q1 = quote { path[User, Role].filter(so => so._1.symbol == "MSFT") }
//   // println( run(q1) )

//   inline def q1 = quote { path[User, Permission].filter(urp => urp._2.name == "GuiUser" && urp._1.name == "Joe") }

//   //inline def q1 = quote { path[User, Permission].filter { case (u,r,p) => u.name == "GuiUser" && r.name == "Joe" } }
//   println( run(q1).string(true) )




//   def main(args: Array[String]): Unit = { //hellooooooooooooooooooooo


//   }
// }
