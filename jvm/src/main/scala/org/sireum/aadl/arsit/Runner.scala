package org.sireum.aadl.arsit

import java.io.File

import org.sireum.aadl.ir.{Aadl, JSON, MsgPack}
import org.sireum.{B, Either, String, Z}

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
        Console.err.println(s"Error reading from '${args(1)}'")
        return
    }

    Runner.run(new File(args(0)), true, json)
  }

  def run(destDir : File, isJson: B, s: org.sireum.String): Int = {
    if (isJson) {
      JSON.toAadl(s) match {
        case Either.Left(m) => run(destDir, m)
        case Either.Right(m) =>
          Console.println(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
          -1
      }
    }
    else {
      org.sireum.conversions.String.fromBase64(s) match {
        case Either.Left(u) =>
          MsgPack.toAadl(u) match {
            case Either.Left(m) => run(destDir, m)
            case Either.Right(m) =>
              Console.println(s"MsgPack deserialization error at offset ${m.offset}: ${m.message}")
              -1
          }
        case Either.Right(m) =>
          Console.println(m)
          -1
      }
    }
  }

  def run(destDir : File, m: Aadl) : Int = run(destDir, m, destDir.getName)

  def run(destDir : File, m: Aadl, basePackageName: String) : Int = {

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
    new File(_destDir, "component").mkdir
    new File(_destDir, "data").mkdir

    val nextPortId: Z = ArtArchitectureGen(new File(_destDir, "architecture"), m, basePackageName)

    ArtStubGenerator(_destDir, m, basePackageName)

    val bin_dir = new File(destDir, "bin")
    val c_dir = new File(destDir, "src/c")
    val nix_dir = new File(_destDir, "nix")
    bin_dir.mkdir()
    nix_dir.mkdir()
    ArtNixGen(nix_dir, c_dir, bin_dir, m, basePackageName, nextPortId)

    0
  }
}
