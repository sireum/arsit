// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{CTranspilerOption, Library, SlangUtil, Util}

object StringTemplate {

  def doNotEditComment(from: Option[String]): String = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return s"// This file was auto-generated${_from}.  Do not edit"
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

  type Bits = org.sireum.ISZ[org.sireum.B]

  @datatype class Boolean_Payload(value: Boolean) extends art.DataContent

  @datatype class Integer_Payload(value: Integer) extends art.DataContent
  @datatype class Integer_8_Payload(value: Integer_8) extends art.DataContent
  @datatype class Integer_16_Payload(value: Integer_16) extends art.DataContent
  @datatype class Integer_32_Payload(value: Integer_32) extends art.DataContent
  @datatype class Integer_64_Payload(value: Integer_64) extends art.DataContent

  @datatype class Unsigned_8_Payload(value: Unsigned_8) extends art.DataContent
  @datatype class Unsigned_16_Payload(value: Unsigned_16) extends art.DataContent
  @datatype class Unsigned_32_Payload(value: Unsigned_32) extends art.DataContent
  @datatype class Unsigned_64_Payload(value: Unsigned_64) extends art.DataContent

  @datatype class Float_Payload(value: Float) extends art.DataContent
  @datatype class Float_32_Payload(value: Float_32) extends art.DataContent
  @datatype class Float_64_Payload(value: Float_64) extends art.DataContent

  @datatype class Character_Payload(value: Character) extends art.DataContent
  @datatype class String_Payload(value: String) extends art.DataContent

  @datatype class Bits_Payload(value: Bits) extends art.DataContent

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
  
  def buildSbt(projectName: String, embedArt: B): ST = {
    val artVersion = Library.getArtVersion()
    val runtimeVersion = Library.getRuntimeVersion()
    val sireumScalacVersion = Library.getSireumScalacVersionVersion()
    val scalaTestVersion = Library.getScalaTestVersion()
    val scalaVersion = Library.getScalaVersion()
    
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

val runtimeVersion = "${runtimeVersion}" // https://github.com/sireum/runtime/tree/${runtimeVersion}

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
    "org.sireum.runtime" %% "library" % runtimeVersion withSources() withJavadoc()
  )
)

val slangEmbeddedSettings = Seq(
  ${embeddedArt._3}
  Compile / unmanagedSourceDirectories += baseDirectory.value / "src/main/architecture",
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
      Seq(
        Compile / unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/bridge",
        libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")
    )        
"""
    return ret
  }

}
