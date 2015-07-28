import sbt._
import sbt.Keys._

object SaladBuild extends Build {

  val saladSettings = Seq(
    organization := "x7c1",
    scalaVersion := "2.11.7",
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
      "org.scalatest" % "scalatest_2.11" % "2.2.1" % Test
    )).
    dependsOn(`salad-lib`)

  lazy val root = Project("salad", file(".")).
    settings(saladSettings:_*).
    aggregate(`salad-lib`, `salad-sample`)

}
