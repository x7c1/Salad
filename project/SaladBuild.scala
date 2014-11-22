import sbt._
import sbt.Keys._

object SaladBuild extends Build {

  val saladSettings = Seq(
    scalaVersion := "2.11.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature")
  )

  lazy val `salad-lib` = project.
    settings(saladSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ))

  lazy val `salad-sample` = project.
    settings(saladSettings:_*).
    settings(libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.13" % Test
    )).
    dependsOn(`salad-lib`)

  lazy val root = Project("salad", file(".")).
    aggregate(`salad-lib`, `salad-sample`)

}
