name := "Scalon"

scalaVersion := "2.10.0"

version := "0.1"

// Compiler flags
scalacOptions ++= Seq("-deprecation", "-feature")

// Application dependencies
libraryDependencies ++= Seq(
    "com.google.code.gson" % "gson" % "2.2.2",
    "org.specs2" %% "specs2" % "1.13" % "test"
)

