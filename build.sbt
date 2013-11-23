name := "Scalon"

organization := "com.roundeights"

version := "0.1"

scalaVersion := "2.10.3"

// Compiler flags
scalacOptions ++= Seq("-deprecation", "-feature")

publishTo := Some("Spikemark" at "https://spikemark.herokuapp.com/maven/roundeights")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// Application dependencies
libraryDependencies ++= Seq(
    "com.google.code.gson" % "gson" % "2.2.2",
    "org.specs2" %% "specs2" % "2.3.4" % "test"
)

