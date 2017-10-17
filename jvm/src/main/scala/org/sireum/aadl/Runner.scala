package org.sireum.aadl

import org.sireum.ST

object Runner {

  def main (args: Array[String]): Unit = {
    import scala.io.Source
    import org.sireum.aadl.ast.JSON
    import org.sireum.{String => SireumString}

    val fileName = System.getProperty("user.home") + "/aadl.json"
    val json = Source.fromFile(fileName).getLines.mkString

    val m = JSON.toMyTop(SireumString(json))
    val a: ST = ArchitectureGen.gen(m)

    println(a.render)
  }
}
