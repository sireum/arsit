package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.hamr.arsit.Util.reporter

object Arsit {
  def run(m: Aadl, o: Cli.ArsitOption, reporter: Reporter) : ArsitResult = {
    Util.reporter = reporter
    Util.verbose = o.verbose
    return runInternal(m, o)
  }

  private def runInternal(m: Aadl, o: Cli.ArsitOption) : ArsitResult = {
    var resources: ISZ[Resource] = ISZ()
    var transpilerOptions: Option[CTranspilerOption] = None()

    if(m.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(resources, 0, 0, transpilerOptions)
    }

    val typeMap = TypeResolver.processDataTypes(m.dataComponents, o.packageName)

    val projectDirectories = ProjectDirectories(o.outputDir)

    val nixPhase = ArtNixGen(projectDirectories, m, o, typeMap,
      ArtStubGenerator(projectDirectories, m, o, typeMap,
        ArtArchitectureGen(projectDirectories, m, o, typeMap)))

    var artResources: ISZ[Resource] = ISZ()
    if (o.embedArt) {
      artResources = Util.copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, projectDirectories.srcMainDir)
    }

    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.transpilerOptions)
  }
}
