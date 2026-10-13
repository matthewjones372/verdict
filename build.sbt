val scala3       = "3.3.6"
val zioTest      = "2.1.14"
val h2           = "2.3.232"

ThisBuild / scalaVersion := scala3
ThisBuild / organization := "dev.verdict"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// The Scala 3 compiler and scaladoc resolve on this build's scala-tool and scala-doc-tool configurations, and
// bring jackson 2.15.1, jsoup 1.17.2 and jline 3.27.1, which have open advisories. Only the build runs them.
val jackson = "2.18.11"
val jline   = "3.30.17"
ThisBuild / dependencyOverrides ++= Seq(
  "com.fasterxml.jackson.core"       % "jackson-core"            % jackson,
  "com.fasterxml.jackson.core"       % "jackson-databind"        % jackson,
  "com.fasterxml.jackson.core"       % "jackson-annotations"     % jackson,
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jackson,
  "com.fasterxml.jackson.datatype"   % "jackson-datatype-jsr310" % jackson,
  "org.jsoup"                        % "jsoup"                   % "1.23.2",
  "org.jline"                        % "jline-reader"            % jline,
  "org.jline"                        % "jline-terminal"          % jline,
  "org.jline"                        % "jline-terminal-jni"      % jline,
  "org.jline"                        % "jline-native"            % jline
)

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
