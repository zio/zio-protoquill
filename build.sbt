

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    resolvers += Resolver.mavenLocal,

    scalaVersion := "0.26.0-RC1", // "0.21.0-RC1", //"0.22.0-bin-20200114-193f7de-NIGHTLY", //dottyLatestNightlyBuild.get,

    scalacOptions ++= Seq(
      "-language:implicitConversions"
    ),

    libraryDependencies ++= Seq(
      ("com.lihaoyi" %% "pprint" % "0.5.6").withDottyCompat(scalaVersion.value),
      ("io.getquill" %% "quill-core-portable" % "3.5.2").withDottyCompat(scalaVersion.value),
      ("io.getquill" %% "quill-sql-portable" % "3.5.2").withDottyCompat(scalaVersion.value),
      ("org.scalameta" %% "scalafmt-dynamic" % "2.7.4").withDottyCompat(scalaVersion.value),
      ("org.scalameta" %% "scalafmt-cli" % "2.7.4").withDottyCompat(scalaVersion.value),
      "ch.epfl.lamp" % "dotty_0.26" % (scalaVersion.value),
      "org.scalatest" % "scalatest_0.26" % "3.2.2" % "test"
    )
  )
