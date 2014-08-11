name := "Salad"

version := "1.0"

scalaVersion := "2.11.2"

lazy val root = (project in file(".")).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.11.2"
    ),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )

lazy val `salad-sample` = project.
  dependsOn(root).
  settings(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.13" % "test"
    ),
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
