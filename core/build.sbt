name := "Connect Four Core"

scalacOptions ++= Common.scalacOptions

libraryDependencies ++= Seq(
  "org.specs2" % "specs2_2.10" % "2.3.10" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.1.2" % "test")

org.scalastyle.sbt.ScalastylePlugin.Settings

scalariformSettings

ScalariformKeys.preferences := Common.defaultScalariformPreferences
