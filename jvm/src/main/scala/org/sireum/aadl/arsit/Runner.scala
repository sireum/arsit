package org.sireum.aadl.arsit

import java.io.File

import org.sireum.{B, String}
import org.sireum.aadl.skema.ast.{Aadl, JSON, MsgPack}
import org.sireum.ops.ISZOps

import scala.io.Source

object Runner {

  def main (args: Array[scala.Predef.String]): Unit = {

    if(args.length != 2) {
      println("Usage: run <dest-dir> <json>")
      return
    }

    var json = ""
    try {
      json = Source.fromFile(args(1)).getLines.mkString
    } catch {
      case e: Throwable =>
        Console.err.println(s"Error reading from '${args(0)}'")
        return
    }

    Runner.run(new File(args(0)), true, json)
  }

  def run(destDir : File, isJson: B, s: org.sireum.String): Int = {
    if (isJson)
      run(destDir, JSON.toAadl(s))
    else
      try
        run(destDir, MsgPack.toAadl(org.sireum.conversions.String.fromBase64(s)))
      catch {
        case e: Throwable =>
          Console.println(e.getMessage)
          -1
      }
  }

  def run(destDir : File, m: Aadl) : Int = {

    if(m.components.isEmpty) {
      Console.err.println("Model is empty")
      return -1
    }

    val _destDir = new File(destDir, "src/main")

    _destDir.mkdirs // try creating the dir structure if it doesn't exist yet

    if(!_destDir.exists){
      Console.err.println(s"Could not create directory: ${_destDir.getAbsolutePath}")
      return -1
    }

    // create subdirs for source directories
    new File(_destDir, "architecture").mkdir
    new File(_destDir, "bridge").mkdir
    new File(_destDir, "data").mkdir
    new File(_destDir, "component").mkdir

    ArtArchitectureGen.generator(new File(_destDir, "architecture"), m)

    ArtStubGenerator.generator(_destDir, m)

    0
  }
}
