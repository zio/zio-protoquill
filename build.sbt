
lazy val baseModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
  `quill-sql`
)

// lazy val dbModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
//   `quill-jdbc`
// )

// lazy val jasyncModules = Seq[sbt.ClasspathDep[sbt.ProjectReference]](
//   `quill-jasync`
// )

lazy val allModules =
  baseModules

val filteredModules = {
  allModules
}

lazy val `quill` = {
  val quill =
    (project in file("."))
      .settings(commonSettings: _*)
      .aggregate(filteredModules.map(_.project): _*)
      .dependsOn(filteredModules: _*)
}


lazy val `quill-sql` = 
  (project in file("quill-sql"))
    .settings(commonSettings: _*)
    .settings(
      resolvers ++= Seq(
        Resolver.mavenLocal,
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
      ),

      libraryDependencies ++= Seq(
        // .excludeAll(ExclusionRule(organization="com.trueaccord.scalapb")
        ("com.lihaoyi" %% "pprint" % "0.5.6").withDottyCompat(scalaVersion.value),
        ("io.getquill" %% "quill-core-portable" % "3.6.1").withDottyCompat(scalaVersion.value),
        ("io.getquill" %% "quill-sql-portable" % "3.6.1").withDottyCompat(scalaVersion.value),
        //("org.scalameta" %% "scalafmt-dynamic" % "2.7.4").withDottyCompat(scalaVersion.value),
        //"org.scala-lang" % "scala3-library_3.0.0-M3" % (scalaVersion.value),

        "org.scalatest" % "scalatest_3.0.0-RC2" % "3.2.7" % "test",
        "org.scalatest" % "scalatest-mustmatchers_3.0.0-RC2" % "3.2.7" % "test"
      ),
      libraryDependencies ++= {
        if (includeFormatter) 
          Seq(("org.scalameta" %% "scalafmt-cli" % "2.7.5" ).excludeAll(ExclusionRule(organization = "org.scala-lang.modules", name = "scala-xml_2.13")).withDottyCompat(scalaVersion.value))
        else
          Seq()
      }
    )

// lazy val `quill-jdbc` =
//   (project in file("quill-jdbc"))
//     .settings(commonSettings: _*)
//     //.settings(mimaSettings: _*)
//     //.settings(jdbcTestingSettings: _*)
//     .dependsOn(`quill-sql` % "compile->compile;test->test")

// Include scalafmt formatter for pretty printing failed queries
val includeFormatter =
  sys.props.getOrElse("formatScala", "false").toBoolean

lazy val commonSettings = /* ReleasePlugin.extraReleaseCommands ++  */ basicSettings

lazy val basicSettings = Seq(
  scalaVersion := "3.0.0-RC2", // "0.21.0-RC1", //"0.22.0-bin-20200114-193f7de-NIGHTLY", //dottyLatestNightlyBuild.get,
  scalacOptions ++= Seq(
    "-language:implicitConversions"
  )
)
