import sbt._
import Keys._

object ApplicationBuild extends Build  {

	private val buildSettings = Project.defaultSettings ++ Seq(
		organization := "scala-demo",
		version := "1.0-SNAPSHOT",
		scalaVersion := "2.9.2",
		shellPrompt := {
			(state: State) => "%s> ".format(Project.extract(state).currentProject.id)
		},
		scalacOptions ++= Seq("-encoding", "UTF-8", "-Xlint","-deprecation", "-unchecked"),
        javacOptions  ++= Seq("-source","1.6","-target","1.6", "-encoding", "UTF-8","-Xlint:unchecked", "-Xlint:deprecation"),
        libraryDependencies ++= Seq(
        		"org.specs2" %% "specs2" % "1.11" % "test"
        	)
	)

	lazy val demo = Project("scala-demo", file("."), settings = buildSettings).settings() 
}