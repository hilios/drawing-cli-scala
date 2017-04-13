name := "springer-challenge"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

logBuffered in Test := false

mainClass := Some("drawing.Main")