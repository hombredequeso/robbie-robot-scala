name := "robbie-robot-scala"

version := "1.0"

scalaVersion := "2.12.1"
scalacOptions += "-language:postfixOps"
scalacOptions += "-feature"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
