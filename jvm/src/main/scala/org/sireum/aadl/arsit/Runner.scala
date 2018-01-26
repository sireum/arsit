package org.sireum.aadl.arsit

import java.io.File

import org.sireum
import org.sireum.aadl.skema.ast.JSON
import org.sireum.ops.ISZOps

import scala.io.Source

object Runner {

  def main (args: Array[String]): Unit = {
    if(args.length != 2) {
      println("Usage: run <json> <dest-dir>")
      return
    }

    var json = ""
    try {
      json = Source.fromFile(args(0)).getLines.mkString
    } catch {
      case e: Throwable =>
        Console.err.println(s"Error reading from '${args(0)}'")
        return
    }

    Runner.run(json, new File(args(1) + "/src/main"))
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
