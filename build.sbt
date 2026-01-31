ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

val akkaVersion = "2.8.8"

lazy val deps = Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion, // For standard log configuration
  "com.typesafe.akka" %% "akka-remote" % akkaVersion, // For akka remote
  "com.typesafe.akka" %% "akka-cluster-typed" % akkaVersion, // akka clustering module
  "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

lazy val clientDeps = deps ++ Seq(
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "com.thesamet.scalapb" %% "scalapb-runtime" % "0.11.17"
)

lazy val commonAssemblySettings = Seq(
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case "reference.conf"              => MergeStrategy.concat
    case x                             => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
  .settings(
    name := "project-cabo-online",
    libraryDependencies ++= deps
  )
  .enablePlugins(MultiJvmPlugin)
  .configs(MultiJvm)
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
    commonAssemblySettings,
    libraryDependencies ++= deps,
    assembly / assemblyJarName := "server.jar",
    assembly / mainClass := Some("ServerApp")
  )
  .dependsOn(commons)

lazy val client = (project in file("cabo-client"))
  .settings(
    name := "project-cabo-client",
    commonAssemblySettings,
    libraryDependencies ++= clientDeps,
    assembly / assemblyJarName := "client.jar",
    assembly / mainClass := Some("ClientApp")
  )
  .enablePlugins(MultiJvmPlugin)
  .configs(MultiJvm)
  .dependsOn(commons)
  .dependsOn(commons)

lazy val seed = (project in file("cabo-seed"))
  .settings(
    name := "project-cabo-seed",
    commonAssemblySettings,
    libraryDependencies ++= deps,
    assembly / assemblyJarName := "seed.jar",
    assembly / mainClass := Some("SeedApp")
  )
  .dependsOn(commons)

// --- TASK TO CREATE JAR E PUT IN ROOT ---
lazy val install = taskKey[Unit]("generate jars")

install := {
  val clientJar = (client / assembly).value
  val serverJar = (server / assembly).value
  val seedJar  = (seed / assembly).value

  val dest = baseDirectory.value

  IO.copyFile(clientJar, dest / "client.jar")
  IO.copyFile(serverJar, dest / "server.jar")
  IO.copyFile(seedJar,  dest / "seed.jar")

  println("\n-------------------------------------------------------")
  println(" SUCCESS! Jars have been generated:")
  println(s" 1. ${(dest / "seed.jar").getPath}")
  println(s" 2. ${(dest / "server.jar").getPath}")
  println(s" 3. ${(dest / "client.jar").getPath}")
  println("-------------------------------------------------------\n")
}