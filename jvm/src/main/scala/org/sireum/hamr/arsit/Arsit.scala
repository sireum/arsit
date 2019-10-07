package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir.Aadl
import org.sireum.message._
import org.sireum.hamr.arsit.Util.reporter

object Arsit {
  def run(m: Aadl, o: Cli.ArsitOption, reporter: Reporter) : ArsitResult = {
    Util.reporter = reporter
    Util.verbose = o.verbose
    return ArsitResult(runInternal(m, o))
  }

  private def runInternal(m: Aadl, o: Cli.ArsitOption) : ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()

    if(m.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return resources
    }

    val typeMap = TypeResolver.processDataTypes(m.dataComponents, o.packageName)

    val projectDirectories = ProjectDirectories(o.outputDir)

    val archPhase: PhaseResult = ArtArchitectureGen(projectDirectories, m, o, typeMap)

    val stubPhase: ISZ[Resource] = ArtStubGenerator(projectDirectories, m, o, typeMap)

    resources = resources ++ archPhase.resources ++ stubPhase

    val maxNixPort: Z =
      if(o.platform != Cli.ArsitPlatform.JVM) {
        val nixPhase = ArtNixGen(projectDirectories, m, archPhase.maxPort, archPhase.maxComponent, o, typeMap)
        resources = resources ++ nixPhase.resources
        nixPhase.maxPort
      }
      else {
        archPhase.maxPort
      }

    if(o.embedArt) {
      resources = resources ++ Util.copyArtFiles(maxNixPort, archPhase.maxComponent, projectDirectories.srcMainDir)
    }

    return resources
  }
}
