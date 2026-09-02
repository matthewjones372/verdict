val scala3       = "3.3.6"
val zioTest      = "2.1.14"
val h2           = "2.3.232"

ThisBuild / scalaVersion := scala3
ThisBuild / organization := "dev.verdict"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// zio-test does not use JUnit, so without this line `sbt test` runs and reports
// nothing at all rather than failing.
ThisBuild / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-explain",
    "-Wunused:imports"
  )
)

// The AST and every interpreter. No macros here, and no dependencies: a rule is
// data, and reading data should not drag a runtime in.
lazy val core = project
  .in(file("core"))
  .settings(commonSettings, name := "verdict-core")

// A macro cannot be expanded in the compilation unit that defines it, so the
// compile-time authoring route needs its own subproject.
lazy val macros = project
  .in(file("macros"))
  .dependsOn(core)
  .settings(commonSettings, name := "verdict-macros")

lazy val tests = project
  .in(file("tests"))
  .dependsOn(core, macros)
  .settings(
    commonSettings,
    name := "verdict-tests",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "dev.zio"    %% "zio-test"          % zioTest % Test,
      "dev.zio"    %% "zio-test-sbt"      % zioTest % Test,
      "dev.zio"    %% "zio-test-magnolia" % zioTest % Test,
      "com.h2database" % "h2"             % h2      % Test
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate(core, macros, tests)
  .settings(commonSettings, name := "verdict", publish / skip := true)
