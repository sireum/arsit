package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols.SymbolResolver
import org.sireum.hamr.codegen.common.types.{TypeResolver, TypeUtil}
import org.sireum.hamr.codegen.common.util.ExperimentalOptions
import org.sireum.hamr.ir
import org.sireum.message._

object Arsit {
  def run(m: ir.Aadl, o: ArsitOptions, reporter: Reporter): ArsitResult = {
    return runInternal(m, o, reporter)
  }

  private def runInternal(m: ir.Aadl, o: ArsitOptions, reporter: Reporter): ArsitResult = {
    var model = m

    if (model.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(ISZ(), 0, 0, ISZ[TranspilerConfig]())
    }

    assert(model.components.size == 1, "Expecting a single root component")

    val result = ir.Transformer(Transformers.MissingTypeRewriter(reporter)).transformAadl(Transformers.CTX(F, F), model)
    model = if (result.resultOpt.nonEmpty) result.resultOpt.get else model

    val rawConnections: B = PropertyUtil.getUseRawConnection(model.components(0).properties)
    val aadlTypes = TypeResolver.processDataTypes(model, rawConnections, o.packageName)

    val useCaseConnectors: B = ExperimentalOptions.useCaseConnectors(o.experimentalOptions)
    val symbolTable = SymbolResolver.resolve(model, Some(o.packageName), useCaseConnectors, aadlTypes, reporter)

    if (!TypeUtil.verifyBitCodec(aadlTypes, symbolTable, reporter)) {
      return ArsitResult(ISZ(), 0, 0, ISZ[TranspilerConfig]())
    }

    val projectDirectories = ProjectDirectories(o.outputDir)

    val nixPhase =
      nix.NixGenDispatch.generate(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes, reporter,
        StubGenerator(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes, reporter,
          ArchitectureGenerator(projectDirectories, symbolTable.rootSystem, o, symbolTable, aadlTypes, reporter).generate()
        ).generate())

    var artResources: ISZ[Resource] = ISZ()
    if (o.embedArt) {
      artResources = copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, projectDirectories.srcMainDir)
    }

    val buildSbtDest = Os.path(o.outputDir) / "build.sbt"
    val projectName = CommonUtil.getLastName(m.components(0).identifier)
    val buildSbtContent = StringTemplate.buildSbt(projectName, o.packageName, o.embedArt)
    artResources = artResources :+ Resource(buildSbtDest.value, buildSbtContent, F, F)

    val buildPropertiesDest = Os.path(o.outputDir) / "project/build.properties"
    artResources = artResources :+ Resource(buildPropertiesDest.value, StringTemplate.sbtBuildPropertiesContents(), F, F)

    val pluginsSbtDest = Os.path(o.outputDir) / "project" / "plugins.sbt"
    artResources = artResources :+ Resource(pluginsSbtDest.value, StringTemplate.sbtPluginsSbtContents(), F, F)

    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.transpilerOptions)
  }

  private def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for ((p, c) <- ArsitLibrary.getFiles if p.native.contains("art")) {
      val _c =
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

      resources = resources :+ Util.createResource(outputDir, ISZ(p), st"${_c}", T)
    }
    return resources
  }
}
