package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.$internal.{RC}

object ArsitLibrary_Ext {

  def getFiles: ISZ[(String, String)] = {
    val map = RC.text(Vector(
      "../../../../../../../../resources/art/src/main/scala/",
      "../../../../../../../../resources/util")) { (p, f) =>
      f.getName.endsWith(".scala") || f.getName.contains("ipc")
    }
    ISZ(map.toSeq.map(p => (String(p._1.mkString("/")), String(p._2))): _*)
  }

  def tripleQuote(): ST = {
    val x = "\"\"\""
    return st"$x"
  }
}