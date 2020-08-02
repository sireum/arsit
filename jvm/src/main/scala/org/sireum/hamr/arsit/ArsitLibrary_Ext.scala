package org.sireum.hamr.arsit

import java.io.StringReader

import org.sireum.$internal.RC
import org.sireum._

object ArsitLibrary_Ext {

  def getFiles: ISZ[(String, String)] = {
    val map = RC.text(Vector(
      "../../../../../../../../resources/art/shared/src/main/scala/",
      "../../../../../../../../resources/util")) { (p, f) => true }
    ISZ(map.toSeq.map(p => (String(p._1.mkString("/")), String(p._2))): _*)
  }


  def getBuildSbtProperties(): java.util.Properties = {
    val str = Map(getFiles).get("buildSbt.properties").get
    val p = new java.util.Properties()
    p.load(new StringReader(str.native))
    return p
  }

  def getArtVersion(): String = {
    return getBuildSbtProperties().getProperty("art.version")
  }

  def getRuntimeVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.runtime.version")
  }

  def getSireumScalacVersionVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scalac-plugin")
  }

  def getScalaVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scala")
  }

  def getScalaTestVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.scalatest")
  }

  def getSBTVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version.sbt")
  }
}