organization := "home"

name := "TreeCut"

version := "0.1.0"

scalaVersion := "2.11.0"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")


resolvers ++= Seq(
  "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "typesafe repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.specs2"          %%  "specs2"            % "2.3.11"    % "test",
  "junit"               %   "junit"             % "4.8.1"     % "test",
  "org.scalatest"       %   "scalatest_2.11"    % "2.1.5",
  "org.scalacheck"      %%  "scalacheck" % "1.12.1"
)