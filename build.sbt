name := """petstore"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlaySwagger)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2,
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.specs2" %% "specs2-scalacheck" % "3.6",
  "org.specs2" %% "specs2-junit" % "3.6.2" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

resolvers += Resolver.mavenLocal

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator

