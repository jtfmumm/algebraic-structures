lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.4"


lazy val commonSettings = Seq(
  // organization := "com.example",
  version := "0.1.0"
  // scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Algebraic Structures",
    libraryDependencies += scalacheck % Test
  )