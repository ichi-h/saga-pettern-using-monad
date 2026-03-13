val scala3Version = "3.8.1"

// 各アプローチで共通の設定
lazy val commonSettings = Seq(
  version                                := "0.1.0-SNAPSHOT",
  scalaVersion                           := scala3Version,
  libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
)

lazy val shared = project
  .in(file("shared"))
  .settings(
    commonSettings,
    name := "shared"
  )

lazy val pattern1 = project
  .in(file("pattern1"))
  .dependsOn(shared)
  .settings(
    commonSettings,
    name := "pattern1"
  )

lazy val root = project
  .in(file("."))
  .aggregate(shared, pattern1)
  .settings(
    name         := "saga-pattern-using-monad",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version
  )
