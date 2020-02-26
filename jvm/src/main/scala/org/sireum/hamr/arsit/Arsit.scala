package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.message._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates._

object Arsit {
  def run(m: ir.Aadl, o: Cli.ArsitOption, reporter: Reporter) : ArsitResult = {
    Util.reporter = reporter
    Util.verbose = o.verbose
    return runInternal(m, o)
  }

  private def runInternal(m: ir.Aadl, o: Cli.ArsitOption) : ArsitResult = {
    var model = m
    
    var resources: ISZ[Resource] = ISZ()
    var transpilerOptions: Option[CTranspilerOption] = None()

    if(model.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(resources, 0, 0, transpilerOptions)
    }

    val result = ir.Transformer(Transformers.MissingTypeRewriter(Util.reporter)).transformAadl(Transformers.CTX(F, F), model)
    model = if(result.resultOpt.nonEmpty) result.resultOpt.get else model
    
    val typeMap = TypeResolver.processDataTypes(model.dataComponents, o.packageName)

    val projectDirectories = ProjectDirectories(o.outputDir)

    val nixPhase = ArtNixGen(projectDirectories, model, o, typeMap,
      ArtStubGenerator(projectDirectories, model, o, typeMap,
        ArtArchitectureGen(projectDirectories, model, o, typeMap)))

    var artResources: ISZ[Resource] = ISZ()
    if (o.embedArt) {
      artResources = Util.copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, projectDirectories.srcMainDir)
    }

    val dest = (Os.path(o.outputDir) / "build.sbt").value
    val projectName = Util.getLastName(m.components(0).identifier)
    val buildSbt = StringTemplate.buildSbt(projectName, o.embedArt)
    artResources = artResources :+ Resource(dest, buildSbt, F, F)

    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.transpilerOptions)
  }
}
