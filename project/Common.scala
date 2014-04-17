import scalariform.formatter.preferences._

object Common {
  val akkaVersion = "2.2.4"
  val playVersion = "2.2.2"
  val scalacOptions = Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature")
  lazy val defaultScalariformPreferences =
    FormattingPreferences().
      setPreference(AlignParameters, true).
      setPreference(DoubleIndentClassDeclaration, true)
}
