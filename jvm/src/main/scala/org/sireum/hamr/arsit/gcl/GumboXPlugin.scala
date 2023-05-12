// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

@record class GumboXPlugin extends BehaviorEntryPointProviderPlugin {
  val name: String = "GumboX Plugin"

  var processedDatatypeInvariants: B = F

  var handledComponents: Set[IdPath] = Set.empty

  var gumboXGen: GumboXGen = GumboXGen()

  override def canHandle(entryPoint: EntryPoints.Type,
                         optInEventPort: Option[AadlPort],
                         component: AadlThreadOrDevice,
                         resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                         arsitOptions: ArsitOptions,
                         symbolTable: SymbolTable,
                         aadlTypes: AadlTypes): B = {
    @pure def hasDatatypeInvariants: B = {
      for (aadlType <- aadlTypes.typeMap.values if GumboXGen.getGclAnnexInfos(ISZ(aadlType.name), symbolTable).nonEmpty) {
        return T
      }
      return F
    }

    val hasGumboSubclauseInfo: B =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        // GCL's symbol resolver ensures there's at most one GCL clause per component
        case ISZ(GclAnnexClauseInfo(annex, _)) =>
          val hasInitContracts: B = annex.initializes.nonEmpty && annex.initializes.get.guarantees.nonEmpty
          val hasComputeContracts: B = annex.compute match {
            case Some(c) => c.cases.nonEmpty || c.specs.nonEmpty || c.handlers.nonEmpty
            case _ => F
          }
          annex.integration.nonEmpty || hasInitContracts || hasComputeContracts
        case _ => F
      }

    return !handledComponents.contains(component.path) && (hasDatatypeInvariants || hasGumboSubclauseInfo)
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
                      arsitOptions: ArsitOptions,
                      reporter: Reporter): BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions = {
    if (!processedDatatypeInvariants) {
      gumboXGen.processDatatypes(basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)
      processedDatatypeInvariants = T
    }

    val gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) => Some((annex, gclSymbolTable))
      case _ => None()
    }

    gumboXGen.processAnnex(component, componentNames,
      gclSubclauseInfo,
      basePackageName, symbolTable, aadlTypes, projectDirectories, reporter)
    handledComponents = handledComponents + component.path

    return BehaviorEntryPointProviderPlugin.emptyPartialContributions
  }

  override def finalise(component: AadlThreadOrDevice,
                        componentNames: NameProvider,
                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                        basePackageName: String,
                        symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        projectDirectories: ProjectDirectories,
                        arsitOptions: ArsitOptions,
                        reporter: Reporter): Option[ObjectContributions] = {

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) => Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    val gumbox = gumboXGen.finalise(component, componentNames, projectDirectories)

    val testHarness = gumboXGen.createTestHarness(component, componentNames, annexInfo, arsitOptions.runSlangCheck, symbolTable, aadlTypes, projectDirectories)

    return Some(gumbox(resources = gumbox.resources ++ testHarness.resources))
  }
}
