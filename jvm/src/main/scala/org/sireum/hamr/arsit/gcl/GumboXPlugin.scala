// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.message.Reporter

@record class GumboXPlugin extends BehaviorEntryPointProviderPlugin {
  val name: String = "GumboX Plugin"

  var handledComponents: Set[IdPath] = Set.empty

  var gumboXGen: GumboXGen = GumboXGen()

  override def canHandle(entryPoint: EntryPoints.Type,
                         optInEventPort: Option[AadlPort],
                         component: AadlThreadOrDevice,
                         resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                         symbolTable: SymbolTable): B = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      // GCL's symbol resolver ensures there's at most one GCL clause per component
      case ISZ(GclAnnexClauseInfo(annex, _)) =>
        return (!handledComponents.contains(component.path) &&
          (annex.integration.nonEmpty || annex.initializes.nonEmpty || annex.compute.nonEmpty))
      case _ => return F
    }
  }

  override def handle(entryPoint: EntryPoints.Type,
                      optInEventPort: Option[AadlPort],
                      component: AadlThreadOrDevice,
                      componentNames: NameProvider,
                      excludeComponentImplementation: B,
                      methodSignature: String,
                      defaultMethodBody: ST,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                      basePackageName: String,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      projectDirectories: ProjectDirectories,
                      reporter: Reporter): BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>
        gumboXGen.processAnnex(component, componentNames, annex, gclSymbolTable, basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)
        handledComponents = handledComponents + component.path
        return BehaviorEntryPointProviderPlugin.emptyPartialContributions
      case _ => halt("Infeasible as handle only called when plugin's canHandle returns T")
    }
  }

  override def finalise(component: AadlThreadOrDevice,
                        componentNames: NameProvider,
                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                        basePackageName: String,
                        symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        projectDirectories: ProjectDirectories,
                        reporter: Reporter): Option[ObjectContributions] = {

    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>
        val gumbox = gumboXGen.finalise(component, componentNames, projectDirectories)

        val testHarness = gumboXGen.createTestHarness(component, componentNames, annex, gclSymbolTable, symbolTable, aadlTypes, projectDirectories)

        return Some(gumbox(resources = gumbox.resources ++ testHarness.resources))
      case _ =>
        return None()
    }
  }
}
