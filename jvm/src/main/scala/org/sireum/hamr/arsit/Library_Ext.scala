package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.$internal.RC
import java.io.StringReader

object Library_Ext {

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

  def getSireumVersion(): String = {
    return getBuildSbtProperties().getProperty("org.sireum.version")
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

  
}