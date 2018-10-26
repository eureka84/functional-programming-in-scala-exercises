lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "it.eureka.fp",
      scalaVersion := "2.12.7"
    )),
    name := "fp-in-scala-exercises"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

