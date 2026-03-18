val scala3Version = "3.3.7"

lazy val commonSettings = Seq(
  version                                := "0.1.0-SNAPSHOT",
  scalaVersion                           := scala3Version,
  libraryDependencies += "org.scalameta" %% "munit"       % "1.0.0" % Test,
  libraryDependencies += "org.typelevel" %% "cats-core"   % "2.13.0",
  libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.0"
)

lazy val shared = project
  .in(file("shared"))
  .settings(
    commonSettings,
    name := "shared"
  )

lazy val stdlibSaga = project
  .in(file("stdlib-saga"))
  .dependsOn(shared)
  .settings(
    commonSettings,
    name := "stdlib-saga"
  )

lazy val catsSaga = project
  .in(file("cats-saga"))
  .dependsOn(shared)
  .settings(
    commonSettings,
    name := "cats-saga"
  )

lazy val root = project
  .in(file("."))
  .aggregate(shared, stdlibSaga, catsSaga)
  .settings(
    name         := "saga-pattern-using-monad",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version
  )
