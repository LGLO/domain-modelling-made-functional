name := "domain-modelling-made-functional"
description := "Domain Modelling Made Functional Book project with Scala"
version := "0.0.1"
// scalaVersion := "0.23.0-RC1"
scalaVersion in ThisBuild := "2.13.1"

scalafmtOnCompile := true

// Scala libraries
val zioDeps = Seq(
  "dev.zio" %% "zio" % Versions.zio
)

lazy val naive = (project in file("naive")).settings(
  libraryDependencies ++= zioDeps
)
lazy val idiomatic = (project in file("idiomatic")).settings(
  libraryDependencies ++= zioDeps
)

lazy val root = project.aggregate(naive, idiomatic)

lazy val Versions = new {
  val zio = "1.0.0-RC18"
}
