ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "3.0.0"

lazy val core = project.in(file("core"))
  .settings(
    name := "incremental-scala",
    libraryDependencies ++= Seq(
      // "core" module - IO, IOApp, schedulers
      // This pulls in the kernel and std modules automatically.
      "org.typelevel" %% "cats-effect" % "3.3.12",
      // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
      "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
      // standard "effect" library (Queues, Console, Random etc.)
      "org.typelevel" %% "cats-effect-std" % "3.3.12"
    ),
    scalacOptions ++= Seq(
      "-no-indent"
    )
  )
lazy val jmh = project.in(file("jmh")).dependsOn(core).enablePlugins(JmhPlugin)
lazy val incremental = (project in file(".")).dependsOn(jmh, core).aggregate(jmh, core)
