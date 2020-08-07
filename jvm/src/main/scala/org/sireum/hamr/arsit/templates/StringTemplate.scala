// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.util.ArsitLibrary

object StringTemplate {

  def doNotEditComment(from: Option[String]): String = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return s"// This file was auto-generated${_from}.  Do not edit"
  }

  def safeToEditComment(): String = {
    return "// This file will not be overwritten so is safe to edit"
  }

  def buildSbt(projectName: String,
               basePackageName: String,
               embedArt: B): ST = {
    val artVersion = ArsitLibrary.getArtVersion()
    val kekinianVersion = ArsitLibrary.getKekinianVersion()
    val runtimeVersion = ArsitLibrary.getRuntimeVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()

    val embeddedArt: (Option[ST], Option[ST], Option[ST]) = if (!embedArt) {
      (Some(st"""val artVersion = "${artVersion}" // https://github.com/sireum/slang-embedded-art/tree/${artVersion}"""),
        Some(st""""org.sireum.slang-embedded-art" %% "slang-embedded-art" % artVersion withSources() withJavadoc(),"""),
        None())
    } else {
      (None(), None(), Some(st"""Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/art","""))
    }

    val ret: ST =
      st"""// Example sbt build definitions -- the contents of this file will not be overwritten
          |//
          |// To open the following project in Sireum IVE select 'File > Open ...' and
          |// navigate to the directory containing this file then click 'OK'.  To install
          |// Sireum IVE see https://github.com/sireum/kekinian#installing
          |//
          |// To run the demo from the command line:
          |//   sbt run
          |//
          |// To run the example unit tests:
          |//   sbt test
          |//
          |// To build a runnable/executable jar:
          |//   sbt assembly
          |//
          |// To skip running the unit tests while building the executable jar:
          |//   sbt 'set test in assembly := {}' assembly
          |// on Linux/Mac, or
          |//   sbt "set test in assembly := {}" assembly
          |// on Windows
          |//
          |// sbt can be obtained from https://www.scala-sbt.org/download.html
          |
          |lazy val ${projectName} = slangEmbeddedTestProject("${projectName}", ".")
          |
          |
          |val scalaVer = "${scalaVersion}"
          |
          |val sireumScalacVersion = "${sireumScalacVersion}" // https://github.com/sireum/scalac-plugin/tree/${sireumScalacVersion}
          |
          |// kekinian commit corresponding to runtime https://github.com/sireum/runtime/tree/${runtimeVersion}
          |val kekinianVersion = "${kekinianVersion}" // https://github.com/sireum/kekinian/tree/${kekinianVersion}
          |
          |val scalaTestVersion = "${scalaTestVersion}"
          |
          |${embeddedArt._1}
          |
          |val commonSettings = Seq(
          |  organization := "org.sireum",
          |  incOptions := incOptions.value.withLogRecompileOnMacro(false),
          |  scalaVersion := scalaVer,
          |  scalacOptions := Seq("-target:jvm-1.8", "-deprecation",
          |    "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings"),
          |  Test / parallelExecution := true,
          |  resolvers ++= Seq(Resolver.sonatypeRepo("public"), "jitpack" at "https://jitpack.io"),
          |  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion),
          |  libraryDependencies ++= Seq(
          |    ${embeddedArt._2}
          |    "org.sireum.kekinian" %% "library" % kekinianVersion withSources() withJavadoc()
          |  )
          |)
          |
          |import sbtassembly.AssemblyPlugin.defaultUniversalScript
          |val slangEmbeddedSettings = Seq(
          |  ${embeddedArt._3}
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/bridge",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/component",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/data",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/nix",
          |  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/seL4Nix",
          |
          |  mainClass in (Compile, run) := Some("${basePackageName}.Demo"),
          |
          |  mainClass in assembly := Some("${basePackageName}.Demo"),
          |  assemblyJarName in assembly := "${projectName}.jar",
          |  assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = false))),
          |
          |  assemblyMergeStrategy in assembly := {
          |    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
          |    case x => MergeStrategy.first
          |  }
          |)
          |
          |def standardProject(projId: String, projectDirectory: String) =
          |  Project(id = projId, base = file(projectDirectory)).settings(commonSettings)
          |
          |def slangEmbeddedProject(projId: String, projectDirectory: String) =
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings)
          |
          |def slangEmbeddedTestProject(projId: String, projectDirectory: String) =
          |  Project(id = projId, base = file(projectDirectory)).
          |    settings(commonSettings ++ slangEmbeddedSettings ++
          |      Seq(
          |        Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
          |        libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")
          |    )
          |"""
    return ret
  }

  def sbtBuildPropertiesContents(): ST = {
    val sbtVersion: String = ArsitLibrary.getSBTVersion()
    val ret: ST =
      st"""sbt.version=${sbtVersion}
          |"""
    return ret
  }

  def sbtPluginsSbtContents(): ST = {
    val sbtAssemblyVersion = ArsitLibrary.getSbtAssemblyVersion()
    val ret: ST =
      st"""addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "${sbtAssemblyVersion}")
          |"""
    return ret
  }
}
