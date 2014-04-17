name := "Connect Four Core"

scalacOptions ++= Common.scalacOptions

org.scalastyle.sbt.ScalastylePlugin.Settings

scalariformSettings

ScalariformKeys.preferences := Common.defaultScalariformPreferences
