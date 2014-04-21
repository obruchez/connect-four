name := "Connect Four Core"

scalacOptions ++= Common.scalacOptions

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.1.2" % "test",
  "org.specs2" % "specs2_2.10" % "2.3.10" % "test",
  "org.scalanlp" % "breeze_2.10" % "0.7",
  "org.scalanlp" % "breeze-natives_2.10" % "0.7")

unmanagedBase <<= baseDirectory { base => base / "lib" }

org.scalastyle.sbt.ScalastylePlugin.Settings

scalariformSettings

ScalariformKeys.preferences := Common.defaultScalariformPreferences
