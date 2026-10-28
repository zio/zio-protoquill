// package zio.quill.context.jdbc

// import com.typesafe.config.ConfigFactory
// import zio.quill.{ JdbcContextConfig, Spec }
// import org.scalatest._

// class JdbcContextConfigSpec extends Spec {
//   "fail if cannot load dataSource" in {
//     intercept[IllegalStateException] {
//       JdbcContextConfig(ConfigFactory.empty()).dataSource
//     }
//   }
// }