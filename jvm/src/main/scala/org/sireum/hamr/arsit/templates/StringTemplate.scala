// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.ArsitLibrary

object StringTemplate {

  def doNotEditComment(from: Option[String]): String = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return s"// This file was auto-generated${_from}.  Do not edit"
  }

  def safeToEditComment(): String = {
    return "// This file will not be overwritten so is safe to edit"
  }

  def Base_Types(basePackage: String): ST = {
    val ret =
      st"""// #Sireum

package ${basePackage}

import org.sireum._
import org.sireum.S8._
import org.sireum.S16._
import org.sireum.S32._
import org.sireum.S64._
import org.sireum.U8._
import org.sireum.U16._
import org.sireum.U32._
import org.sireum.U64._

${doNotEditComment(None())}

object Base_Types {

  type Boolean = org.sireum.B

  type Integer = org.sireum.Z

  type Integer_8 = org.sireum.S8
  type Integer_16 = org.sireum.S16
  type Integer_32 = org.sireum.S32
  type Integer_64 = org.sireum.S64

  type Unsigned_8 = org.sireum.U8
  type Unsigned_16 = org.sireum.U16
  type Unsigned_32 = org.sireum.U32
  type Unsigned_64 = org.sireum.U64

  // TODO: Base_Types::Natural

  type Float = org.sireum.R
  type Float_32 = org.sireum.F32
  type Float_64 = org.sireum.F64

  type Character = org.sireum.C
  type String = org.sireum.String

  type Bits = org.sireum.ISZ[B]

  @datatype class Boolean_Payload(value: B) extends art.DataContent

  @datatype class Integer_Payload(value: Z) extends art.DataContent
  
  @datatype class Integer_8_Payload(value: S8) extends art.DataContent
  @datatype class Integer_16_Payload(value: S16) extends art.DataContent
  @datatype class Integer_32_Payload(value: S32) extends art.DataContent
  @datatype class Integer_64_Payload(value: S64) extends art.DataContent

  @datatype class Unsigned_8_Payload(value: U8) extends art.DataContent
  @datatype class Unsigned_16_Payload(value: U16) extends art.DataContent
  @datatype class Unsigned_32_Payload(value: U32) extends art.DataContent
  @datatype class Unsigned_64_Payload(value: U64) extends art.DataContent

  @datatype class Float_Payload(value: R) extends art.DataContent
  @datatype class Float_32_Payload(value: F32) extends art.DataContent
  @datatype class Float_64_Payload(value: F64) extends art.DataContent

  @datatype class Character_Payload(value: C) extends art.DataContent
  @datatype class String_Payload(value: String) extends art.DataContent

  @datatype class Bits_Payload(value: ISZ[B]) extends art.DataContent

  def Boolean_empty(): Boolean = { return F }

  def Integer_empty(): Integer = { return z"0" }

  def Integer_8_empty(): Integer_8 = { return s8"0" }
  def Integer_16_empty(): Integer_16 = { return s16"0" }
  def Integer_32_empty(): Integer_32 = { return s32"0" }
  def Integer_64_empty(): Integer_64 = { return s64"0" }

  def Unsigned_8_empty(): Unsigned_8 = { return u8"0" }
  def Unsigned_16_empty(): Unsigned_16 = { return u16"0" }
  def Unsigned_32_empty(): Unsigned_32 = { return u32"0" }
  def Unsigned_64_empty(): Unsigned_64 = { return u64"0" }

  def Float_empty(): Float = { return r"0" }
  def Float_32_empty(): Float_32 = { return f32"0" }
  def Float_64_empty(): Float_64 = { return f64"0" }

  def Character_empty(): Character = { return ' ' }
  def String_empty(): String = { return "" }

  def Bits_empty(): Bits = { return ISZ() }
}"""
    return ret
  }
  
  def buildSbt(projectName: String, 
               embedArt: B): ST = {
    val artVersion = ArsitLibrary.getArtVersion()
    val runtimeVersion = ArsitLibrary.getRuntimeVersion()
    val sireumScalacVersion = ArsitLibrary.getSireumScalacVersionVersion()
    val scalaTestVersion = ArsitLibrary.getScalaTestVersion()
    val scalaVersion = ArsitLibrary.getScalaVersion()

    val embeddedArt: (Option[ST], Option[ST], Option[ST]) = if(!embedArt) {
      (Some(st"""val artVersion = "${artVersion}" // https://github.com/sireum/slang-embedded-art/tree/${artVersion}"""),
        Some(st""""org.sireum.slang-embedded-art" %% "slang-embedded-art" % artVersion withSources() withJavadoc(),"""),
        None())
    } else { (None(), None(), Some(st"""Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/art",""")) }
    
    val ret: ST =
      st"""
// Example sbt build definitions -- the contents of this file will not be overwritten
//
// To open the following project in Sireum IVE select 'File > Open ...' and then
// navigate to the directory containing this file then click 'OK'.  To install
// Sireum IVE see https://github.com/sireum/kekinian#installing

lazy val ${projectName} = slangEmbeddedTestProject("${projectName}", ".")


val scalaVer = "${scalaVersion}"

val sireumScalacVersion = "${sireumScalacVersion}" // https://github.com/sireum/scalac-plugin/tree/${sireumScalacVersion}

val runtimeVersion = "${runtimeVersion}" // https://github.com/sireum/kekinian/tree/${runtimeVersion}

val scalaTestVersion = "${scalaTestVersion}"

${embeddedArt._1}

val commonSettings = Seq(
  organization := "org.sireum",
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalaVersion := scalaVer,
  scalacOptions := Seq("-target:jvm-1.8", "-deprecation",
    "-Ydelambdafy:method", "-feature", "-unchecked", "-Xfatal-warnings"),
  Test / parallelExecution := true,
  resolvers ++= Seq(Resolver.sonatypeRepo("public"), "jitpack" at "https://jitpack.io"),
  addCompilerPlugin("org.sireum" %% "scalac-plugin" % sireumScalacVersion),
  libraryDependencies ++= Seq(
    ${embeddedArt._2}
    "org.sireum.kekinian" %% "library" % runtimeVersion withSources() withJavadoc()
  )
)

val slangEmbeddedSettings = Seq(
  ${embeddedArt._3}
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/bridge",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/component",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/data",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/nix",
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/seL4Nix" 
)

def standardProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).settings(commonSettings)

def slangEmbeddedProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).
    settings(commonSettings ++ slangEmbeddedSettings)

def slangEmbeddedTestProject(projId: String, projectDirectory: String) =
  Project(id = projId, base = file(projectDirectory)).
    settings(commonSettings ++ slangEmbeddedSettings ++
      Seq(
        Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
        libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")
    )        
"""
    return ret
  }
  
  def sbtProject(): ST = {
    val sbtVersion: String = ArsitLibrary.getSBTVersion()
    return st"""sbt.version=${sbtVersion}
               |"""
  }
}
