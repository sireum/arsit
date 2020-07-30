package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.message._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.SymbolResolver
import org.sireum.hamr.codegen.common.types.{TypeResolver, TypeUtil}
import org.sireum.hamr.codegen.common.util.ExperimentalOptions

object Arsit {
  def run(m: ir.Aadl, o: Cli.ArsitOption, reporter: Reporter) : ArsitResult = {
    Util.reporter = reporter
    Util.verbose = o.verbose
    return runInternal(m, o)
  }

  private def runInternal(m: ir.Aadl, o: Cli.ArsitOption) : ArsitResult = {
    var model = m
    
    var resources: ISZ[Resource] = ISZ()

    if(model.components.isEmpty) {
      reporter.error(None(), Util.toolName, "Model is empty")
      return ArsitResult(resources, 0, 0, ISZ[CTranspilerOption]())
    }

    val result = ir.Transformer(Transformers.MissingTypeRewriter(Util.reporter)).transformAadl(Transformers.CTX(F, F), model)
    model = if(result.resultOpt.nonEmpty) result.resultOpt.get else model

    val useCaseConnectors: B = ExperimentalOptions.useCaseConnectors(o.experimentalOptions)
    val symbolTable = SymbolResolver.resolve(model, Some(o.packageName), useCaseConnectors, reporter)

    val aadlTypes = TypeResolver.processDataTypes(model, symbolTable, o.packageName)

    if(!TypeUtil.verifyBitCodec(aadlTypes, symbolTable, reporter)){
      return ArsitResult(resources, 0, 0, ISZ[CTranspilerOption]())
    }

    val projectDirectories = ProjectDirectories(o.outputDir)

    val nixPhase = nix.NixGenDispatch.generate(projectDirectories, model, o, symbolTable, aadlTypes,
      StubGenerator(projectDirectories, model, o, symbolTable, aadlTypes,
        ArchitectureGenerator(projectDirectories, model, o, symbolTable, aadlTypes)))

    var artResources: ISZ[Resource] = ISZ()
    if (o.embedArt) {
      artResources = Util.copyArtFiles(nixPhase.maxPort, nixPhase.maxComponent, projectDirectories.srcMainDir)
    }

    val dest = Os.path(o.outputDir) / "build.sbt"
    val projectName = CommonUtil.getLastName(m.components(0).identifier)
    val buildSbt = StringTemplate.buildSbt(projectName, o.embedArt)
    artResources = artResources :+ Resource(dest.value, buildSbt, F, F)
    
    val projectFile = StringTemplate.sbtProject()
    val projectFileDest = Os.path(o.outputDir) / "project/build.properties"
    artResources = artResources :+ Resource(projectFileDest.value, projectFile, F, F)
    
    return ArsitResult(nixPhase.resources ++ artResources,
      nixPhase.maxPort,
      nixPhase.maxComponent,
      nixPhase.transpilerOptions)
  }
}
