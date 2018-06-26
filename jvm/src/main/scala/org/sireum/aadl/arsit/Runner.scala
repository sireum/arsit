package org.sireum.aadl.arsit

import java.io.File

import org.sireum.aadl.ir.{Aadl, JSON, MsgPack}
import org.sireum.cli.Sireum.path2fileOpt
import org.sireum.{B, F, T, Z, ISZ, Either, String, Some}
import org.sireum.cli.Cli

object Runner {

  // hint: execute something like "run --fakeopt" to print the help
  def main (args: Array[scala.Predef.String]): Unit = {
    val cli = Cli(_root_.java.io.File.pathSeparatorChar)
    cli.parseArsit(ISZ(args.map(s => s: String): _*), Z(0)) match {
      case Some(o: Cli.ArsitOption) => run(o)
      case _ => println(cli.parseArsit(ISZ(), 0).get.asInstanceOf[Cli.ArsitOption].help)
    }
  }

  def run(o: org.sireum.cli.Cli.ArsitOption): Int = {
    val destDir = path2fileOpt("output directory", o.outputDir, T).get
    if (!destDir.isDirectory) {
      println(s"Path ${destDir.getPath} is not a directory")
      return -1
    }

    val inputFile = path2fileOpt("input file", o.inputFile, F)
    val input = if (inputFile.nonEmpty) {
      scala.io.Source.fromFile(inputFile.get).getLines.mkString
    } else {
      println("Reading from stdin.  Enter Ctrl+d on a blank line to finish")
      var s, l = ""
      while ({ l = scala.io.StdIn.readLine; l != null }) s += l
      s
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

    val basePackageName : String = if (o.packagename.nonEmpty) o.packagename.get else destDir.getName()

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
