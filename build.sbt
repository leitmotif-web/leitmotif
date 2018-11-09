import ReleaseTransformations._

scalaVersion in ThisBuild := "2.12.7"
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  releaseStepCommandAndRemaining("publishSigned"),
  releaseStepCommand("sonatypeReleaseAll"),
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)

val core =
  pro("core")
    .settings(
      libraryDependencies ++= List(
        "org.typelevel" %% "cats-free" % "1.1.0",
        "io.chrisdavenport" %% "log4cats-slf4j" % "0.1.0",
        "ch.qos.logback" % "logback-classic" % "1.2.3",
        "com.lihaoyi" %% "scalatags" % "0.6.7",
      )
    )

val root =
  basicProject(project.in(file(".")))
    .aggregate(core)
    .settings(noPublish)
