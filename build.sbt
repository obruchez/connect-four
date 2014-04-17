name := "Connect Four"

scalaVersion in ThisBuild := "2.10.4"

version in ThisBuild := "1.0-SNAPSHOT"

play.Project.playScalaSettings

lazy val root = project.in(file(".")).aggregate(core, web)

lazy val core = project.in(file("core"))

lazy val web = project.in(file("web"))
