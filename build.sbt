ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

ThisBuild / resolvers += "Akka library repository".at("https://repo.akka.io/maven")

val akkaVersion = "2.9.3"

lazy val deps = Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion, // For standard log configuration
  "com.typesafe.akka" %% "akka-remote" % akkaVersion, // For akka remote
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion, // akka clustering module
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

lazy val root = (project in file("."))
  .settings(
    name := "project-cabo-online",
    libraryDependencies ++= deps
  )
  .aggregate(client, server)
  .dependsOn(client, server)

lazy val commons = (project in file("cabo-commons"))
  .settings(
    name := "project-cabo-commons",
    libraryDependencies ++= deps
  )

lazy val server = (project in file("cabo-server"))
  .settings(
    name := "project-cabo-server",
    libraryDependencies ++= deps
  )
  .dependsOn(commons)

lazy val clientDeps = deps ++ Seq(
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
)

lazy val client = (project in file("cabo-client"))
  .settings(
    name := "project-cabo-client",
    libraryDependencies ++= clientDeps
  )
  .dependsOn(commons)