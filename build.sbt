lazy val root = Project(id = "kantan-tests", base = file("."))
  .settings(moduleName := "root")
  .settings(publish / skip := true)
  .aggregate(core, sbtTests)

lazy val core = project
  .settings(
    moduleName := "kantan.tests",
    name       := "core",
    Test / unmanagedClasspath ++= (LocalProject("sbt") / Compile / fullClasspath).value,
    testFrameworks += TestFramework("kantan.tests.sbt.Framework")
  )

lazy val sbtTests = Project(id = "sbt", base = file("sbt"))
  .settings(
    moduleName                            := "kantan.tests-sbt",
    name                                  := "sbt",
    libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"
  )
  .dependsOn(core)

lazy val demo = project
  .settings(
    moduleName := "kantan.tests-demo",
    name       := "demo",
    testFrameworks += TestFramework("kantan.tests.sbt.Framework")
  )
  .dependsOn(core, sbtTests % Test)
