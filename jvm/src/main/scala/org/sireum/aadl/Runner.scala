package org.sireum.aadl

import java.io.File

import org.sireum
import org.sireum.aadl.ast.JSON
import org.sireum.ops.ISZOps

import scala.io.Source

object Runner {

  def main (args: Array[String]): Unit = {
    var pname = "building-control-gen"
    //var pname = "pca-pump-gen"

    val fileName = System.getProperty("user.home") + "/aadl.json"
    val json = Source.fromFile(fileName).getLines.mkString
    val destDir = new File(s"/Users/belt/devel/sireum2/slang-embedded/${pname}/src/main")

    Runner.run(json, destDir)
  }

  def run(json: String, destDir : File): Unit = {
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

    ArtStubGenerator.generator(destDir, m)
  }
}
