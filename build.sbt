name := "smtp"

version := "0.0.1"

versionScheme := Some("early-semver")

scalaVersion := "3.1.3"

enablePlugins(ScalaNativePlugin)

nativeLinkStubs := true

nativeMode := "debug"

nativeLinkingOptions := Seq(s"-L${baseDirectory.value}/native-lib")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
)

organization := "io.github.edadma"

githubOwner := "edadma"

githubRepository := name.value

Global / onChangedBuildSource := ReloadOnSourceChanges

resolvers += Resolver.githubPackages("edadma")

resolvers += Resolver.githubPackages("spritzsn")

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % "test"

libraryDependencies ++= Seq(
  //  "com.lihaoyi" %%% "pprint" % "0.7.2", /*% "test"*/
  "io.github.cquiroz" %%% "scala-java-time" % "2.4.0",
)

libraryDependencies ++= Seq(
  "io.github.spritzsn" %%% "libuv" % "0.0.21",
  "io.github.spritzsn" %%% "fs" % "0.0.6",
)

publishMavenStyle := true

Test / publishArtifact := false
