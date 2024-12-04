ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2024"
  )

libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
