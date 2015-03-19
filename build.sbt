name := "scala_crypt"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-Xelide-below", "ALL")

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.9",
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.specs2" %% "specs2" % "2.4.1"
)
