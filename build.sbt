name := "bug-squash"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.assembla.scala-incubator" %% "graph-core" % "1.7.3",
  "net.sf.supercsv" % "super-csv" % "2.1.0"
)     

scalaVersion := "2.10.3"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)

play.Project.playScalaSettings
