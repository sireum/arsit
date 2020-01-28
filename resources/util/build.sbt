// Example sbt build definitions
//
// To open the following project in Sireum IVE select 'File > Open ...' and then
// navigate to the directory containing this file then click 'OK'

lazy val example_test_project = slangEmbeddedTestProject("example_test_project", ".")


val scalaVer = "2.12.10"

val sireumScalacVersion = "4.20191127.dd7cc5c"

val runtimeVersion = "edfe6c2"

val commonSettings = Seq(
  organization := "org.sireum",
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalaVersion := scalaVer,
  scalacOptions := Seq("-target:jvm-1.8", "-deprecation",
    "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings"),
  Test / parallelExecution := true,
  resolvers ++= Seq(Resolver.sonatypeRepo("public"), "jitpack" at "https://jitpack.io"),
  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion),
  libraryDependencies += "org.sireum.runtime" %% "library" % runtimeVersion
)

val slangEmbeddedSettings = Seq(
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/art",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/bridge",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/component",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/data",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/nix"
)

def standardProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).settings(commonSettings)

def slangEmbeddedProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).
    settings(commonSettings ++ slangEmbeddedSettings)

def slangEmbeddedTestProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).
    settings(commonSettings ++ slangEmbeddedSettings ++
      Seq(Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test")
    )