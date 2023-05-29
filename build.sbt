ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "cats-beginner"
  )

val Http4sVersion = "0.23.19"
val BlazeHttp4sVersion = "0.23.15"
val CirceVersion = "0.14.5"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % BlazeHttp4sVersion,
  "org.http4s" %% "http4s-circe"        % Http4sVersion,
  "org.http4s" %% "http4s-dsl"          % Http4sVersion,
  "io.circe"   %% "circe-generic"       % CirceVersion
)
