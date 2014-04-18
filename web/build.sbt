name := "Connect Four Web"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)     

play.Project.playScalaSettings

org.scalastyle.sbt.ScalastylePlugin.Settings

scalariformSettings

ScalariformKeys.preferences := Common.defaultScalariformPreferences
