import ReleaseTransformations._
//import com.typesafe.sbt.SbtScalariform.ScalariformKeys
//import scalariform.formatter.preferences._
import com.jsuereth.sbtpgp.PgpKeys.publishSigned
import sbtrelease.ReleasePlugin
import scala.sys.process.Process
import java.io.{File => JFile}

ThisBuild / versionScheme := Some("always")

// During release cycles, GPG will expect passphrase user-input EVEN when --passphrase is specified
// this should add --pinentry-loopback in order to disable that. See here for more info:
// https://github.com/sbt/sbt-pgp/issues/178
Global / useGpgPinentry := true

releaseVersion     := { ver =>
  println(s"=== Releasing on initially specified version: ${ver}")
  ver
}
releaseNextVersion := { ver =>
  val withoutLast = ver.reverse.dropWhile(_.isDigit).reverse
  val last = ver.reverse.takeWhile(_.isDigit).reverse
  println(s"=== Detected original version: ${ver}. Which is ${withoutLast} + ${last}")
  // see if the last group of chars are numeric, if they are, just increment
  val actualLast = scala.util.Try(last.toInt).map(i => (i + 1).toString).getOrElse(last)
  val newVer = withoutLast + actualLast + "-SNAPSHOT"
  println(s"=== Final computed version is: ${newVer}")
  newVer
}

lazy val isCommunityBuild =
  sys.props.getOrElse("community", "false").toBoolean

lazy val isCommunityRemoteBuild =
  sys.props.getOrElse("communityRemote", "false").toBoolean

lazy val scalatestVersion =
  if (isCommunityRemoteBuild) "3.2.7" else "3.2.9"

lazy val baseModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-sql`
)

lazy val sqlTestModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-sql-tests`
)

lazy val dbModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-jdbc`, `quill-zio`, `quill-jdbc-zio`, `quill-caliban`
)

lazy val jasyncModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-jasync`, `quill-jasync-postgres`
)

lazy val bigdataModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-cassandra`, `quill-cassandra-zio`
)

lazy val allModules =
  baseModules ++ sqlTestModules ++ dbModules ++ jasyncModules ++ bigdataModules

lazy val communityBuildModules =
  Seq[sbt.ClasspathDep[sbt.ProjectReference]](
    `quill-sql`, `quill-sql-tests`
  )

val filteredModules = {
  val modulesStr = sys.props.get("modules")
  println(s"Modules Argument Value: ${modulesStr}")

  val modules = modulesStr match {
    case Some("base") =>
      println("SBT =:> Compiling Base Modules")
      baseModules
    case Some("sqltest") =>
      println("SBT =:> Compiling SQL test Modules")
      sqlTestModules
    case Some("db") =>
      println("SBT =:> Compiling Database Modules")
      dbModules
    case Some("async") =>
      println("SBT =:> Compiling Async Database Modules")
      jasyncModules
    case Some("bigdata") =>
      println("SBT =:> Compiling Big Data Modules")
      bigdataModules
    case Some("none") =>
      println("SBT =:> Invoking Aggregate Project")
      Seq[sbt.ClasspathDep[sbt.ProjectReference]]()
    case _ =>
      println("SBT =:> No Modules Switch Specified, Compiling All Modules by Default")
      allModules
  }

  val filteredModules =
    if(isCommunityBuild) {
      println("SBT =:> Doing Community Build! Filtering Community-Build Modules Only")
      modules.filter(communityBuildModules.contains(_))
    } else
      modules

  println(s"=== Selected Modules ===\n${filteredModules.map(_.project.toString).toList.mkString("\n")}\n=== End Selected Modules ===")
  filteredModules
}

lazy val `quill` = {
  (project in file("."))
    .settings(commonSettings: _*)
    // Unless release settings bubbled up here, they won't actually be used for the project
    // release. E.g. if you don't want to run tests on a release (i.e. if they were run on a previous step)
    // and release-settings here are not included tests will still be run etc...
    .settings(releaseSettings: _*)
    .aggregate(filteredModules.map(_.project): _*)
    .dependsOn(filteredModules: _*)
    .settings(
      publishArtifact := false,
      publish / skip := true,
      publishLocal / skip := true,
      publishSigned / skip := true,
    )
}

lazy val `quill-sql` =
  (project in file("quill-sql"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      resolvers ++= Seq(
        Resolver.mavenLocal,
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
        "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
      ),
      excludeDependencies ++= Seq(
        "com.typesafe.scala-logging" % "scala-logging_2.13"
      ),
      libraryDependencies ++= Seq(
        // Needs to be in-sync with both quill-engine and scalafmt-core or ClassNotFound
        // errors will happen. Even if the pprint classes are actually there
        ("com.lihaoyi" %% "pprint" % "0.6.6"),
        "io.getquill" %% "quill-engine" % "3.16.0",
        ("io.getquill" %% "quill-util" % "3.16.0")
          .excludeAll({
            if (isCommunityBuild)
              Seq(ExclusionRule(organization = "org.scalameta", name = "scalafmt-core_2.13"))
            else
              Seq()
          }: _*),
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
        "org.scalatest" %% "scalatest" % scalatestVersion % Test,
        "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
        "com.vladsch.flexmark" % "flexmark-all" % "0.35.10" % Test
      )//,
      //Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oGF")
    )

// Moving heavy tests to separate module so it can be compiled in parallel with others
lazy val `quill-sql-tests` =
  (project in file("quill-sql-tests"))
    .settings(commonSettings: _*)
    .dependsOn(`quill-sql` % "compile->compile;test->test")

//lazy val `quill-sql-all` = (project in file(".")).aggregate(`quill-sql`, `quill-sql-tests`)

lazy val `quill-jdbc` =
  (project in file("quill-jdbc"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(jdbcTestingSettings: _*)
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-jasync` =
  (project in file("quill-jasync"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.github.jasync-sql" % "jasync-common" % "1.1.4"
      )
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-jasync-postgres` =
  (project in file("quill-jasync-postgres"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.github.jasync-sql" % "jasync-postgresql" % "1.1.4"
      )
    )
    .dependsOn(`quill-jasync` % "compile->compile;test->test")

lazy val `quill-caliban` =
  (project in file("quill-caliban"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.github.ghostdogpr" %% "caliban" % "1.2.4",
        "com.github.ghostdogpr" %% "caliban-zio-http"   % "1.2.4",
        // Adding this to main dependencies would force users to use logback-classic for SLF4j unless the specifically remove it
        // seems to be safer to just exclude & add a commented about need for a SLF4j implementation in Docs.
        "ch.qos.logback" % "logback-classic" % "1.2.3" % Test,
        "io.d11" %% "zhttp"      % "1.0.0.0-RC17" % Test,
        // Don't want to make this dependant on zio-test for the testing code so importing this here separately
        "org.scalatest" %% "scalatest" % scalatestVersion % Test,
        "org.scalatest" %% "scalatest-mustmatchers" % scalatestVersion % Test,
        "org.postgresql"          %  "postgresql"              % "42.2.18"             % Test,
      )
    )
    .dependsOn(`quill-jdbc-zio` % "compile->compile")

lazy val `quill-zio` =
  (project in file("quill-zio"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "dev.zio" %% "zio" % "1.0.12",
        "dev.zio" %% "zio-streams" % "1.0.12"
      )
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-jdbc-zio` =
  (project in file("quill-jdbc-zio"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(jdbcTestingLibraries: _*)
    .settings(
       Test / runMain / fork := true,
       Test / fork := true,
       Test / testGrouping := {
        (Test / definedTests).value map { test =>
          if (test.name endsWith "IntegrationSpec")
            Tests.Group(name = test.name, tests = Seq(test), runPolicy = Tests.SubProcess(
              ForkOptions().withRunJVMOptions(Vector("-Xmx200m"))
            ))
          else
            Tests.Group(name = test.name, tests = Seq(test), runPolicy = Tests.SubProcess(ForkOptions()))
        }
      }
    )
    .dependsOn(`quill-zio` % "compile->compile;test->test")
    .dependsOn(`quill-sql` % "compile->compile;test->test")
    .dependsOn(`quill-jdbc` % "compile->compile;test->test")

lazy val `quill-cassandra` =
  (project in file("quill-cassandra"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := false,
      libraryDependencies ++= Seq(
        "com.datastax.oss" % "java-driver-core" % "4.13.0"
      )
    )
    .dependsOn(`quill-sql` % "compile->compile;test->test")

lazy val `quill-cassandra-zio` =
  (project in file("quill-cassandra-zio"))
    .settings(commonSettings: _*)
    .settings(releaseSettings: _*)
    .settings(
      Test / fork := true,
      libraryDependencies ++= Seq(
        "com.datastax.oss" % "java-driver-core" % "4.13.0",
        "dev.zio" %% "zio" % "1.0.12",
        "dev.zio" %% "zio-streams" % "1.0.12",
        ("dev.zio" %% "zio-interop-guava" % "30.1.0.3").excludeAll(ExclusionRule(organization = "dev.zio")).cross(CrossVersion.for3Use2_13)
      )
    )
    .dependsOn(`quill-cassandra` % "compile->compile;test->test")
    .dependsOn(`quill-zio` % "compile->compile;test->test")

// Include scalafmt formatter for pretty printing failed queries
val includeFormatter =
  sys.props.getOrElse("formatScala", "false").toBoolean

lazy val commonSettings = /* ReleasePlugin.extraReleaseCommands ++  */ basicSettings

lazy val jdbcTestingLibraries = Seq(
  libraryDependencies ++= Seq(
    "com.zaxxer"              %  "HikariCP"                % "3.4.5",
    "mysql"                   %  "mysql-connector-java"    % "8.0.22"             % Test,
    "com.h2database"          %  "h2"                      % "1.4.200"            % Test,
    "org.postgresql"          %  "postgresql"              % "42.2.18"             % Test,
    "org.xerial"              %  "sqlite-jdbc"             % "3.32.3.2"             % Test,
    "com.microsoft.sqlserver" %  "mssql-jdbc"              % "7.1.1.jre8-preview" % Test,
    "com.oracle.ojdbc"        %  "ojdbc8"                  % "19.3.0.0"           % Test,
    //"org.mockito"             %% "mockito-scala-scalatest" % "1.16.2"              % Test
  )
)

lazy val jdbcTestingSettings = jdbcTestingLibraries ++ Seq(
  Test / fork := true
)

lazy val basicSettings = Seq(
  // ,
  //        ("org.scala-lang.modules" %% "scala-java8-compat" % "1.0.1")
  libraryDependencies ++= Seq(
    ("org.scala-lang.modules" %% "scala-java8-compat" % "1.0.1")
  ),
  excludeDependencies ++= Seq(
    ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13")
  ),
  scalaVersion := {
    if (isCommunityBuild) dottyLatestNightlyBuild().get else "3.0.2"
  },
  organization := "io.getquill",
  // The -e option is the 'error' report of ScalaTest. We want it to only make a log
  // of the failed tests once all tests are done, the regular -o log shows everything else.
  // Test / testOptions ++= Seq(
  //   Tests.Argument(TestFrameworks.ScalaTest, "-oF")
  //   //  /*, "-eGNCXEHLOPQRM"*/, "-h", "target/html", "-u", "target/junit"
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-u", "junits")
  //   //Tests.Argument(TestFrameworks.ScalaTest, "-h", "testresults")
  // ),
  scalacOptions ++= Seq(
    "-language:implicitConversions",
  )
)

lazy val releaseSettings = ReleasePlugin.extraReleaseCommands ++ Seq(
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pgpSecretRing := file("local.secring.gpg"),
  pgpPublicRing := file("local.pubring.gpg"),
  releaseVersionBump := sbtrelease.Version.Bump.Next,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseProcess := {
    Seq[ReleaseStep]() ++
    doOnDefault(checkSnapshotDependencies) ++
    doOnDefault(inquireVersions) ++
    doOnDefault(runClean) ++
    doOnPush   (setReleaseVersion) ++
    doOnPush   (commitReleaseVersion) ++
    doOnPush   (tagRelease) ++
    doOnDefault(publishArtifacts) ++
    doOnPush   (setNextVersion) ++
    doOnPush   (commitNextVersion) ++
    //doOnPush(releaseStepCommand("sonatypeReleaseAll")) ++
    doOnPush   (pushChanges)
  },
  homepage := Some(url("http://github.com/getquill/protoquill")),
  licenses := List(("Apache License 2.0", url("https://raw.githubusercontent.com/getquill/protoquill/master/LICENSE.txt"))),
  developers := List(
    Developer("deusaquilus", "Alexander Ioffe", "", url("https://github.com/deusaquilus"))
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/getquill/protoquill"), "git:git@github.com:getquill/protoquill.git")
  )
)

def doOnDefault(steps: ReleaseStep*): Seq[ReleaseStep] =
  Seq[ReleaseStep](steps: _*)

def doOnPush(steps: ReleaseStep*): Seq[ReleaseStep] =
  if (skipPush)
    Seq[ReleaseStep]()
  else
    Seq[ReleaseStep](steps: _*)

val skipPush =
  sys.props.getOrElse("skipPush", "false").toBoolean
