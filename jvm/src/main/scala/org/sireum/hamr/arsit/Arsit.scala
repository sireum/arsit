// #Sireum
package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, ReporterUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.codegen.common.containers.{Resource, SireumToolsSlangcheckOption, TranspilerConfig}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir
import org.sireum.message._

object Arsit {

  //=================================================================
  //  A r s i t    C o d e    G e n e r a t i o n   P i p e l i n e
  //
  //   Primary methods for invoke pipeline phases.
  //   Phase results accumulated and held in memory using the PhaseResult structure,
  //   which is threaded through phases (intermedidate versions are
  //   named according to the associated phase).
  //=================================================================

  def run(model: ir.Aadl, o: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin], reporter: Reporter): ArsitResult = {
    ReporterUtil.resetReporter()
    val ret = runInternal(model, o, aadlTypes, symbolTable, plugins)
    ReporterUtil.addReports(reporter)
    return ret
  }

  def runInternal(model: ir.Aadl, arsitOptions: ArsitOptions, aadlTypes: AadlTypes, symbolTable: SymbolTable, plugins: MSZ[Plugin]): ArsitResult = {

    if (model.components.isEmpty) {
      ReporterUtil.reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(ISZ(), 0, 0, 0, ISZ[TranspilerConfig](), ISZ[SireumToolsSlangcheckOption]())
    }

    assert(model.components.size == 1, "Expecting a single root component")

    val projectDirectories = ProjectDirectories(arsitOptions)

    val nixPhase =
      nix.NixGenDispatch.generate(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes,
        StubGenerator(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes, plugins,
          ArchitectureGenerator(projectDirectories, symbolTable.rootSystem, arsitOptions, symbolTable, aadlTypes, plugins).generate()
        ).generate())

    var artResources: ISZ[Resource] = ISZ()
    if (!arsitOptions.noEmbedArt) {
      artResources = copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, nixPhase.maxConnection, s"${projectDirectories.mainDir}/art")
    }

    artResources = artResources ++ createBuildArtifacts(
      CommonUtil.getLastName(model.components(0).identifier), arsitOptions, projectDirectories, nixPhase.resources, ReporterUtil.reporter)

    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.maxConnection,
      nixPhase.transpilerOptions,
      nixPhase.slangCheckOptions)
  }

  def copyArtFiles(maxPort: Z, maxComponent: Z, maxConnections: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for (entry <- ArsitLibrary.getFiles if ops.StringOps(entry._1).contains("art")) {
      val _c: String =
        if (ops.StringOps(entry._1).contains("Art.scala")) {
          @strictpure def atLeast0(i: Z): Z = if(i < 0) 0 else i
          val subs: ISZ[(String, String)] = ISZ(
            ("@range(min = 0, index = T) class BridgeId", s"  @range(min = 0, max = ${atLeast0(maxComponent - 1)}, index = T) class BridgeId"),
            ("@range(min = 0, index = T) class PortId", s"  @range(min = 0, max = ${atLeast0(maxPort - 1)}, index = T) class PortId"),
            ("@range(min = 0, index = T) class ConnectionId", s"  @range(min = 0, max = ${atLeast0(maxConnections - 1)}, index = T) class ConnectionId"),
            ("val numComponents", s"  val numComponents: Z = $maxComponent"),
            ("val numPorts:", s"  val numPorts: Z = $maxPort"),
            ("val numConnections:", s"  val numConnections: Z = $maxConnections")
          )
          def sub(str: String): String = {
            for(s <- subs if ops.StringOps(str).contains(s._1)) {
              return s._2
            }
            return str
          }
          val lines = StringUtil.split_PreserveEmptySegments(entry._2, (c: C) => c == '\n').map((s: String) => sub(s))
          st"${(lines, "\n")}".render
        } else {
          entry._2
        }

      resources = resources :+ ResourceUtil.createStringResource(Util.pathAppend(outputDir, ISZ(entry._1)), _c, T)
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
      if (candidate.nonEmpty) root.relativize(Os.path(candidate(0).dstPath)).value
      else "??"
    }

    val bridgeTestPath: String = root.relativize(Os.path(projDirs.testBridgeDir)).value

    def dewindowfy(s: String): String = {
      // want path seps in build.sbt comments to be nix so they don't break reg tests
      return ops.StringOps.replaceAllLiterally(conversions.String.toCis(s), "\\", "/")
    }

    val proyekBuildDest = options.outputDir / "bin" / "project.cmd"
    val proyekBuildContent = ProjectTemplate.proyekBuild(projectName, options.packageName, !options.noEmbedArt,
      dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
    ret = ret :+ ResourceUtil.createExeCrlfResource(proyekBuildDest.value, proyekBuildContent, F)

    val versionPropDest = options.outputDir / "versions.properties"
    val versionPropBuildContent = ProjectTemplate.proyekVersionProperties()
    ret = ret :+ ResourceUtil.createResource(versionPropDest.value, versionPropBuildContent, F)

    if (options.genSbtMill) {
      val millBuildDest = options.outputDir / "build.sc"
      val outputDirSimpleName = millBuildDest.up.name
      val millBuildContent = ProjectTemplate.millBuild(options.packageName, outputDirSimpleName, !options.noEmbedArt)
      ret = ret :+ ResourceUtil.createResource(millBuildDest.value, millBuildContent, F)

      val sbtBuildDest = options.outputDir / "build.sbt"
      val sbtBuildContent = ProjectTemplate.sbtBuild(projectName, options.packageName, !options.noEmbedArt,
        dewindowfy(demoScalaPath), dewindowfy(bridgeTestPath))
      ret = ret :+ ResourceUtil.createResource(sbtBuildDest.value, sbtBuildContent, F)

      val buildPropertiesDest = options.outputDir / "project" / "build.properties"
      ret = ret :+ ResourceUtil.createResource(buildPropertiesDest.value, ProjectTemplate.sbtBuildPropertiesContents(), F)

      val pluginsSbtDest = options.outputDir / "project" / "plugins.sbt"
      ret = ret :+ ResourceUtil.createResource(pluginsSbtDest.value, ProjectTemplate.sbtPluginsSbtContents(), F)
    }

    reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
      ProjectTemplate.arsitSlangInstructionsMessage(options.outputDir.value, options.genSbtMill).render)

    if (isNixProject(options.platform)) {
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
        ProjectTemplate.arsitCInstructionsMessage(cmakeDir, devDir, transpile, compile, run, stop).render)
    }

    if (options.platform == ArsitPlatform.SeL4) {
      val transpile: String = {
        val x = resources.filter(p => ops.StringOps(p.dstPath).endsWith("bin/transpile-sel4.cmd"))
        if (x.nonEmpty) x(0).dstPath
        else "??"
      }

      reporter.info(None(), Util.ARSIT_INSTRUCTIONS_MESSAGE_KIND,
        ProjectTemplate.arsitCAmkESInstructionsMessage(projDirs.cExt_c_Dir, transpile).render)
    }

    return ret
  }

  def isNixProject(value: ArsitPlatform.Type): B = {
    val ret: B = value match {
      case ArsitPlatform.Linux => T
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Cygwin => T
      case _ => F
    }
    return ret
  }
}
