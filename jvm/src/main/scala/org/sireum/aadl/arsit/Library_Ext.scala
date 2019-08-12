package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.$internal.{RC}

object Library_Ext {

  def getFiles: ISZ[(String, String)] = {
    val map = RC.text(Vector(
      "../../../../../../../../resources/art/src/main/scala/",
      "../../../../../../../../resources/util")) { (p, f) => true  }
    ISZ(map.toSeq.map(p => (String(p._1.mkString("/")), String(p._2))): _*)
  }
}