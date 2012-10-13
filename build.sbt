name := "Scalon"

scalaVersion := "2.9.2"

version := "0.1"

// append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// Application dependencies
libraryDependencies ++= Seq(
    "com.google.code.gson" % "gson" % "2.2.2",
    "org.specs2" %% "specs2" % "1.12.2" % "test"
)

