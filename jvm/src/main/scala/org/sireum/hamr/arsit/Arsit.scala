package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, ReporterUtil}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir
import org.sireum.message._

object Arsit {
  def run(model: ir.Aadl, o: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ArsitResult = {
    ReporterUtil.resetReporter()
    val ret = runInternal(model, o, aadlTypes, symbolTable)
    ReporterUtil.addReports(reporter)
    return ret
  }

  private def runInternal(model: ir.Aadl, o: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable): ArsitResult = {

    if (model.components.isEmpty) {
      ReporterUtil.reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(ISZ(), 0, 0, ISZ[TranspilerConfig]())
    }

    assert(model.components.size == 1, "Expecting a single root component")

    val projectDirectories = ProjectDirectories(o)

    val nixPhase =
      nix.NixGenDispatch.generate(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes,
        StubGenerator(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes,
          ArchitectureGenerator(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes).generate()
        ).generate())

    var artResources: ISZ[Resource] = ISZ()
    if (!o.noEmbedArt) {
      artResources = copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, s"${projectDirectories.mainDir}/art")
    }

    artResources = artResources ++ createBuildArtifacts(
      CommonUtil.getLastName(model.components(0).identifier), o, projectDirectories, nixPhase.resources, ReporterUtil.reporter)

    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.transpilerOptions)
  }

  private def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for ((p, c) <- ArsitLibrary.getFiles if p.native.contains("art")) {
      val _c: String =
        if (p.native.contains("Art.scala")) {
          val out = new StringBuilder()
          c.native.split("\n").map(s =>
            out.append({
              if (s.contains("val maxComponents")) {
                s"  val maxComponents: BridgeId = $maxComponent"
              } else if (s.contains("val maxPorts:")) {
                s"  val maxPorts: PortId = $maxPort"
              }
              else {
                s
              }
            }).append("\n"))
          out.toString()
        } else {
          c
        }

      resources = resources :+ ResourceUtil.createStringResource(Util.pathAppend(outputDir, ISZ(p)), _c, T)
    }
    return resources
  }

  def createBuildArtifacts(projectName: String,
                           options: ArsitOptions,
                           projDirs: ProjectDirectories,
                           resources: ISZ[Resource],
                           reporter: Reporter): ISZ[Resource] = {

    var ret: ISZ[Resource] = ISZ()
    val root = options.outputDir

    val demoScalaPath: String = {
      val candidate: ISZ[Resource] = resources.filter(p => ops.StringOps(p.dstPath).endsWith("Demo.scala"))
      if(candidate.nonEmpty) root.relativize(Os.path(candidate(0).dstPath)).value
      else "??"
    }

    val bridgeTestPath: String = root.relativize(Os.path(projDirs.testBridgeDir)).value

    def dewindowfy(s: String): String = {
      // want path seps in build.sbt comments to be nix so they don't break reg tests
      return ops.StringOps.replaceAllLiterally(conversions.String.toCis(s), "\\", "/")
    }

    val proyekBuildDest = options.outputDir / "bin" / "project.cmd"
    val proyekBuildContent = StringTemplate.proyekBuild(projectName, options.packageName, !options.noEmbedArt,
      dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
    ret = ret :+ ResourceUtil.createExeCrlfResource(proyekBuildDest.value, proyekBuildContent, F)

    val versionPropDest = options.outputDir / "versions.properties"
    val versionPropBuildContent = StringTemplate.proyekVersionProperties()
    ret = ret :+ ResourceUtil.createResource(versionPropDest.value, versionPropBuildContent, F)

    val millBuildDest = options.outputDir / "build.sc"
    val outputDirSimpleName = millBuildDest.up.name
    val millBuildContent = StringTemplate.millBuild(options.packageName, outputDirSimpleName, !options.noEmbedArt)
    ret = ret :+ ResourceUtil.createResource(millBuildDest.value, millBuildContent, F)

    val sbtBuildDest = options.outputDir / "build.sbt"
    val sbtBuildContent = StringTemplate.sbtBuild(projectName, options.packageName, !options.noEmbedArt,
      dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
    ret = ret :+ ResourceUtil.createResource(sbtBuildDest.value, sbtBuildContent, F)

    val buildPropertiesDest = options.outputDir / "project/build.properties"
    ret = ret :+ ResourceUtil.createResource(buildPropertiesDest.value, StringTemplate.sbtBuildPropertiesContents(), F)

    val pluginsSbtDest = options.outputDir / "project" / "plugins.sbt"
    ret = ret :+ ResourceUtil.createResource(pluginsSbtDest.value, StringTemplate.sbtPluginsSbtContents(), F)

    reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
      StringTemplate.arsitSlangInstructionsMessage(options.outputDir.value).render)

    if(isNixProject(options.platform)) {
      val cmakeDir: String = projDirs.cNixDir

      val devDir = projDirs.cExt_c_Dir

      val transpile: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).endsWith("bin/transpile.cmd"))
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val compile: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).contains("bin/compile.cmd"))
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val run: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).contains("bin/run.sh"))
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      val stop: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).endsWith("bin/stop.sh"))
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
        StringTemplate.arsitCInstructionsMessage(cmakeDir, devDir, transpile, compile, run, stop).render)
    }

    if(options.platform == ArsitPlatform.SeL4) {
      val transpile: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).endsWith("bin/transpile-sel4.cmd"))
        if(x.nonEmpty) x(0).dstPath
        else "??"
      }

      reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
        StringTemplate.arsitCAmkESInstructionsMessage(projDirs.cExt_c_Dir, transpile).render)
    }

    return ret
  }

  def isNixProject(value: ArsitPlatform.Type): B = {
    return value match {
      case ArsitPlatform.Linux => T
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Cygwin => T
      case _ => F
    }
  }
}
