scalaVersion := "3.8.1"
scalacOptions ++= Seq(
  "-source",
  "future",
  "-Xkind-projector:underscores",
  "-deprecation",
  "-experimental",
  "-Yexplicit-nulls",
  "-language:experimental.captureChecking"
)
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
