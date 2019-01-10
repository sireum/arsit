package org.sireum.aadl.arsit

import java.io.File

import org.sireum.aadl.ir.{Aadl, JSON, MsgPack}
import org.sireum.{B, F, T, Z, ISZ, Either, String, Option, Some, None}

object Arsit {

  def run(o: Cli.ArsitOption): Int = {
    val outDir: String = if(o.outputDir.nonEmpty) o.outputDir.get else "."

    val destDir: File = new File(outDir.native)
    if(!destDir.exists()) {
      if(!destDir.mkdirs()){
        println(s"Could not create directory ${destDir.getPath}")
        return -1
      }
    }
    if (!destDir.isDirectory) {
      println(s"Path ${destDir.getPath} is not a directory")
      return -1
    }

    val inputFile:Option[File] = if(o.args.size != 1) None[File] else Some(new File(o.args(0).native))
    val input = if (inputFile.nonEmpty && inputFile.get.exists) {
      scala.io.Source.fromFile(inputFile.get).getLines.mkString
    } else {
      println("Input file not found.  Expecting exactly 1")
      return -1
    }

    if (o.json) {
      JSON.toAadl(input) match {
        case Either.Left(m) => run(destDir, m, o)
        case Either.Right(m) =>
          Console.println(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
          -1
      }
    }
    else {
      org.sireum.conversions.String.fromBase64(input) match {
        case Either.Left(u) =>
          MsgPack.toAadl(u) match {
            case Either.Left(m) => run(destDir, m, o)
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

  def run(destDir : File, m: Aadl, o: Cli.ArsitOption) : Int = {

    if(m.components.isEmpty) {
      Console.err.println("Model is empty")
      return -1
    }

    val basePackageName : String = if (o.packageName.nonEmpty) o.packageName.get else destDir.getName()

    val _destDir = new File(destDir, "src/main")

    _destDir.mkdirs // try creating the dir structure if it doesn't exist yet

    if(!_destDir.exists){
      Console.err.println(s"Could not create directory: ${_destDir.getAbsolutePath}")
      return -1
    }

    // create subdirs for source directories
    new File(_destDir, "architecture").mkdir
    new File(_destDir, "art").mkdir
    new File(_destDir, "bridge").mkdir
    new File(_destDir, "component").mkdir
    new File(_destDir, "data").mkdir

    val (nextPortId, nextComponentId) = ArtArchitectureGen(new File(_destDir, "architecture"), m, basePackageName, o)

    ArtStubGenerator(_destDir, m, basePackageName, o)

    val maxNixPort: Z =
      if(o.genTrans) ArtNixGen(_destDir, m, basePackageName, nextPortId, nextComponentId, o)
      else nextPortId

    if(!o.noart) {
      Util.copyArtFiles(maxNixPort, nextComponentId, _destDir)
    }

    0
  }
}
