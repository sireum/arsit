package org.sireum.hamr.arsit

import java.io.File

import org.sireum.hamr.ir.{Aadl, JSON, MsgPack}
import org.sireum.{B, F, T, Z, ISZ, Either, String, Option, Some, None}

object Arsit {

  def run(o: Cli.ArsitOption): Int = {
    val outDir: String = if(o.outputDir.nonEmpty) o.outputDir.get else "."

    val destDir: File = new File(outDir.native)
    if(!destDir.exists()) {
      if(!destDir.mkdirs()){
        Util.reportError(s"Could not create directory ${destDir.getPath}")
        return -1
      }
    }
    if (!destDir.isDirectory) {
      Util.reportError(s"Path ${destDir.getPath} is not a directory")
      return -1
    }

    val inputFile:Option[File] = if(o.args.size != 1) None[File] else Some(new File(o.args(0).native))
    val input = if (inputFile.nonEmpty && inputFile.get.exists) {
      scala.io.Source.fromFile(inputFile.get).getLines.mkString
    } else {
      Util.reportError("Input file not found.  Expecting exactly 1")
      return -1
    }

    if (o.json) {
      JSON.toAadl(input) match {
        case Either.Left(m) => run(destDir, m, o)
        case Either.Right(m) =>
          Util.reportError(s"Json deserialization error at (${m.line}, ${m.column}): ${m.message}")
          -1
      }
    }
    else {
      org.sireum.conversions.String.fromBase64(input) match {
        case Either.Left(u) =>
          MsgPack.toAadl(u) match {
            case Either.Left(m) => run(destDir, m, o)
            case Either.Right(m) =>
              Util.reportError(s"MsgPack deserialization error at offset ${m.offset}: ${m.message}")
              -1
          }
        case Either.Right(m) =>
          Util.reportError(m)
          -1
      }
    }
  }

  // old arsit osate plugin interface
  def run(model: Aadl, //
          optOutputDir: Option[scala.Predef.String], //
          optBasePackageName: Option[scala.Predef.String], //
          embedArt: B, //
          genBlessEntryPoints: B, //
          genTranspilerArtficats: B, //
          ipcMechanism: ArsitBridge.IPCMechanism): Int = {
    val verbose = true
    val devicesAsThreads = true
    val excludeImpl = false
    val behaviorDir = None[Predef.String]()
    val cdir = None[Predef.String]()
    val platform = if(genTranspilerArtficats) ArsitBridge.Platform.Linux else ArsitBridge.Platform.JVM
    val bitWidth = 64
    val maxStringSize = 256
    val maxArraySize = 16

    run(model, optOutputDir, optBasePackageName, embedArt, genBlessEntryPoints, //
      verbose, //
      devicesAsThreads, //
      // transpiler options
      ipcMechanism, //
      excludeImpl, //
      behaviorDir, //
      cdir, //
      platform, //
      bitWidth, //
      maxStringSize, //
      maxArraySize)
  }


  def run(model: Aadl, //
          optOutputDir: Option[scala.Predef.String], //
          optBasePackageName: Option[scala.Predef.String], //
          embedArt: B, //
          genBlessEntryPoints: B, //
          verbose: B, //
          devicesAsThreads: B, //
          // transpiler options
          ipcMechanism: ArsitBridge.IPCMechanism, //
          excludeImpl: B, //
          behaviorDir: Option[scala.Predef.String], //
          cdir: Option[scala.Predef.String], //
          platform: ArsitBridge.Platform, //
          bitWidth: Int, //
          maxStringSize: Int, //
          maxArraySize: Int): Int = {
    val outDir: String = if(optOutputDir.nonEmpty) optOutputDir.get else "."

    val destDir: File = new File(outDir.native)
    if(!destDir.exists()) {
      if(!destDir.mkdirs()){
        Util.reportError(s"Could not create directory ${destDir.getPath}")
        return -1
      }
    }
    if (!destDir.isDirectory) {
      Util.reportError(s"Path ${destDir.getPath} is not a directory")
      return -1
    }

    val opt = Cli.ArsitOption(
      help = "",
      args = ISZ(),
      json = T,
      outputDir = if(optOutputDir.nonEmpty) Some(optOutputDir.get) else None(),
      packageName = if(optBasePackageName.nonEmpty) Some(optBasePackageName.get) else None(),
      noart = !embedArt,
      bless = genBlessEntryPoints,
      verbose = verbose,
      devicesAsThreads = devicesAsThreads,
      ipc = Cli.IpcMechanism.byName(ipcMechanism.name()).get,
      behaviorDir = if(behaviorDir.nonEmpty) Some(behaviorDir.get) else None(),
      outputCDir = if(cdir.nonEmpty) Some(cdir.get) else None(),
      excludeImpl = excludeImpl,
      platform = Cli.Platform.byName(platform.name()).get,
      bitWidth = bitWidth,
      maxStringSize = maxStringSize,
      maxArraySize = maxArraySize
    )
    return run(destDir, model, opt)
  }

  def run(destDir : File, m: Aadl, o: Cli.ArsitOption) : Int = {
    Util.verbose = o.verbose

    if(m.components.isEmpty) {
      Util.reportError("Model is empty")
      return -1
    }

    val basePackageName : String = if (o.packageName.nonEmpty) o.packageName.get else destDir.getName()

    val typeMap = TypeResolver.processDataTypes(m.dataComponents, basePackageName)

    val _destDir = new File(destDir, "src/main")

    _destDir.mkdirs // try creating the dir structure if it doesn't exist yet

    if(!_destDir.exists){
      Util.reportError(s"Could not create directory: ${_destDir.getAbsolutePath}")
      return -1
    }

    // create subdirs for source directories
    new File(_destDir, "architecture").mkdir
    new File(_destDir, "art").mkdir
    new File(_destDir, "bridge").mkdir
    new File(_destDir, "component").mkdir
    new File(_destDir, "data").mkdir

    val (nextPortId, nextComponentId) = ArtArchitectureGen(new File(_destDir, "architecture"), m, basePackageName, o, typeMap)

    ArtStubGenerator(_destDir, m, basePackageName, o, typeMap)

    val maxNixPort: Z =
      if(o.platform != Cli.Platform.JVM) {
        ArtNixGen(_destDir, m, basePackageName, nextPortId, nextComponentId, o, typeMap)
      }
      else {
        nextPortId
      }

    if(!o.noart) {
      Util.copyArtFiles(maxNixPort, nextComponentId, _destDir)
    }

    0
  }
}
