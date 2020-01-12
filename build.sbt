

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyLatestNightlyBuild.get,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      ("com.lihaoyi" %% "pprint" % "0.5.6").withDottyCompat(scalaVersion.value),
      "ch.epfl.lamp" % "dotty_0.22" % (scalaVersion.value)
    )
  )
