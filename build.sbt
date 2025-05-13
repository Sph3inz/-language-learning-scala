name := "Language Learning Bot"
version := "1.0"
scalaVersion := "2.13.16"

// Add dependency scheme to resolve version conflicts
libraryDependencySchemes += "org.scala-lang.modules" %% "scala-parser-combinators" % "always"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.12" % Test,
  "com.typesafe.akka" %% "akka-http" % "10.2.9",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.9",
  "com.typesafe.akka" %% "akka-stream" % "2.6.19",
  "com.typesafe.akka" %% "akka-actor" % "2.6.19",
  "io.spray" %% "spray-json" % "1.3.6"
)

// Add eviction warning settings
evictionWarningOptions in update := EvictionWarningOptions.default
  .withWarnTransitiveEvictions(false)
  .withWarnDirectEvictions(false)
