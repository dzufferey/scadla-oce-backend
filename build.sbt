name := "scadla-oce-backend"

organization := "com.github.dzufferey"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.4"

scalacOptions in Compile ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature"
)

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "com.github.dzufferey" %% "misc-scala-utils" % "1.0.0",
  "com.github.dzufferey" %% "scadla" % "0.1.0"
)

fork := true
