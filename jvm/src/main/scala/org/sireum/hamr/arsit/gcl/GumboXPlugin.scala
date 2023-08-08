// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.plugin.{BehaviorEntryPointProviderPlugin, EntryPointProviderPlugin}
import org.sireum.hamr.arsit.templates.{EntryPointTemplate, StringTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

@record class GumboXPlugin
  extends EntryPointProviderPlugin with BehaviorEntryPointProviderPlugin {

  val name: String = "GumboX Plugin"

  var processedDatatypeInvariants: B = F

  var handledComponents: Set[IdPath] = Set.empty

  var gumboXGen: GumboXGen = GumboXGen()

  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes): B = {

    @pure def hasDatatypeInvariants: B = {
      for (aadlType <- aadlTypes.typeMap.values if GumboXGen.getGclAnnexInfos(ISZ(aadlType.name), symbolTable).nonEmpty) {
        return T
      }
      return F
    }

    val componentHasGumboSubclauseInfo: B =
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

    return !handledComponents.contains(component.path) && (hasDatatypeInvariants || componentHasGumboSubclauseInfo)
  }

  def handle(component: AadlThreadOrDevice,
             componentNames: NameProvider,

             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,
             projectDirectories: ProjectDirectories,
             reporter: Reporter): Unit = {
    if (!processedDatatypeInvariants) {
      gumboXGen.processDatatypes(componentNames.basePackage, symbolTable, aadlTypes, projectDirectories, reporter)
      processedDatatypeInvariants = T
    }

    val gclSubclauseInfo: Option[(GclSubclause, GclSymbolTable)] =
      resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
        case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) => Some((annex, gclSymbolTable))
        case _ => None()
      }

    gumboXGen.processAnnex(component, componentNames,
      gclSubclauseInfo,
      componentNames.basePackage, symbolTable, aadlTypes, projectDirectories, reporter)

    handledComponents = handledComponents + component.path
  }

  /******************************************************************************************
  * EntryPoint provider
  ******************************************************************************************/

  override def canHandleEntryPointProvider(component: AadlThreadOrDevice,
                                           resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                           arsitOptions: ArsitOptions,
                                           symbolTable: SymbolTable,
                                           aadlTypes: AadlTypes): B = {
    return canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
  }

  override def handleEntryPointProvider(component: AadlThreadOrDevice,
                                        componentNames: NameProvider,
                                        ports: ISZ[Port],

                                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                        entryPointTemplate: EntryPointTemplate,

                                        symbolTable: SymbolTable,
                                        aadlTypes: AadlTypes,
                                        projectDirectories: ProjectDirectories,
                                        reporter: Reporter): EntryPointProviderPlugin.EntryPointContributions = {
    val epCompanionName: String = s"${componentNames.componentSingletonType}_EntryPoint_Companion"

    handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, reporter)

    var companionBlocks: ISZ[ST] = ISZ()
    var companionExtBlocks: ISZ[ST] = ISZ()
    var resources: ISZ[FileResource] = ISZ()

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) => Some((annex_, gclSymbolTable_))
      case _ => None()
    }

    val (inContainer, outContainer) = GumboXGenUtil.genContainers(component, componentNames, annexInfo, aadlTypes)
    val containersPath = s"${projectDirectories.dataDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}_Containers.scala"
    val containerST = DSCTemplate.genTestVectorContainerClass(
      packageName = componentNames.packageName,
      imports = ISZ(s"${componentNames.basePackage}._"),
      containers = ISZ(inContainer._2, outContainer._2)
    )
    resources = resources :+ ResourceUtil.createResourceH(containersPath, containerST, T, T)

    val preInitMethodName = s"pre_${EntryPoints.initialise.name}"
    val postInitMethodName = s"post_${EntryPoints.initialise.name}"

    val initBody =
      st"""${epCompanionName}.${preInitMethodName}()
          |
          |${entryPointTemplate.defaultInitialiseBody}
          |
          |${epCompanionName}.${postInitMethodName}()"""


    val preComputeMethodName = s"pre_${EntryPoints.compute.name}"
    val postComputeMethodName = s"post_${EntryPoints.compute.name}"
    val computeBody =
      st"""${epCompanionName}.${preComputeMethodName}()
          |
          |${entryPointTemplate.defaultComputeBody}
          |
          |${epCompanionName}.${postComputeMethodName}()"""

    val gEntryPoint = entryPointTemplate.generateCustomST(
      blocks = ISZ(),
      activateBody = None(),
      initialiseBody = Some(initBody),
      testInitialiseBody = None(),
      computeBody = Some(computeBody),
      testComputeBody = None(),
      deactivateBody = None(),
      finaliseBody = None(),
      recoverBody = None()
    )

    val epCompanion =
      st"""@ext object ${epCompanionName} {
          |  def ${preInitMethodName}(): Unit = $$
          |  def ${postInitMethodName}(): Unit = $$
          |
          |  def ${preComputeMethodName}(): Unit = $$
          |  def ${postComputeMethodName}(): Unit = $$
          |}"""

    val epCompanionExt =
      st"""package ${componentNames.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${componentNames.basePackage}._
          |
          |${StringTemplate.doNotEditComment()}
          |
          |object ${epCompanionName}_Ext {
          |  def ${preInitMethodName}(): Unit = {
          |    // assume/require contracts cannot refer to incoming ports or
          |    // state variables so nothing to do here
          |  }
          |
          |  def ${postInitMethodName}(): Unit = {
          |
          |  }
          |
          |  def ${preComputeMethodName}(): Unit = {}
          |
          |  def ${postComputeMethodName}(): Unit = {}
          |}"""

    val path = s"${projectDirectories.bridgeDir}/${componentNames.packagePath}/${epCompanionName}_Ext.scala"
    val epCompanionExtR =
      ResourceUtil.createResource(path, epCompanionExt, T)

    return EntryPointProviderPlugin.EntryPointContributions(
      imports = ISZ(),
      bridgeCompanionBlocks = ISZ(epCompanion.render),
      entryPoint = gEntryPoint,
      resources = resources :+ epCompanionExtR
    )
  }

  /******************************************************************************************
  * Behavior EntryPoint provider
  ******************************************************************************************/

  override def canHandleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                                   optInEventPort: Option[AadlPort],
                                                   component: AadlThreadOrDevice,
                                                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                   arsitOptions: ArsitOptions,
                                                   symbolTable: SymbolTable,
                                                   aadlTypes: AadlTypes): B = {
    return canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
  }

  override def handleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type,
                                                optInEventPort: Option[AadlPort],
                                                component: AadlThreadOrDevice,
                                                componentNames: NameProvider,
                                                excludeComponentImplementation: B,

                                                methodSignature: String,
                                                defaultMethodBody: ST,

                                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                                symbolTable: SymbolTable,
                                                aadlTypes: AadlTypes,
                                                projectDirectories: ProjectDirectories,
                                                arsitOptions: ArsitOptions,
                                                reporter: Reporter): BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions = {

    handle(component, componentNames, resolvedAnnexSubclauses, symbolTable, aadlTypes, projectDirectories, reporter)

    return BehaviorEntryPointProviderPlugin.emptyPartialContributions
  }

  override def finaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                  componentNames: NameProvider,
                                                  resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

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
