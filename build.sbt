organization := "sydney.edu.au"

version := "0.1"

name := "chisel-utils"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
