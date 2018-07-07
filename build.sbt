name := "args_parser"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += Classpaths.typesafeReleases

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scalaz" %% "scalaz-core" % "7.2.24",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
