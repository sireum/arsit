package org.sireum.aadl

import java.io.File
import org.sireum.aadl.ast.JSON
import scala.io.Source

object Runner {

  def main (args: Array[String]): Unit = {

    val fileName = System.getProperty("user.home") + "/aadl.json"
    val json = Source.fromFile(fileName).getLines.mkString
    val destDir = new File("/Users/belt/devel/sireum/slang-embedded/pca-pump-gen-v2/src/main")

    Runner.run(json, destDir, false)
  }

  def run(json: String, destDir : File, replaceUserImplementations: Boolean): Unit = {
    destDir.mkdirs // try creating the dir structure if it doesn't exist yet

    if(!destDir.exists){
      println(s"Directory doesn't exist: ${destDir.getAbsolutePath}")
      return
    }

    // create subdirs for source directories
    new File(destDir, "architecture").mkdir
    new File(destDir, "bridge").mkdir
    new File(destDir, "data").mkdir
    new File(destDir, "component").mkdir

    val m = JSON.toAadlXml(org.sireum.String(json))

    ArtArchitectureGen.generator(new File(destDir, "architecture"), m)

    ArtStubGenerator.generator(destDir, m, replaceUserImplementations)
  }
}
