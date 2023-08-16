// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.plugin.{AppProviderPlugin, BehaviorEntryPointProviderPlugin, EntryPointProviderPlugin}
import org.sireum.hamr.arsit.templates.{ApiTemplate, EntryPointTemplate, StringTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

@record class GumboXPlugin
  extends EntryPointProviderPlugin with BehaviorEntryPointProviderPlugin with AppProviderPlugin {

  val name: String = "GumboX Plugin"

  var processedDatatypeInvariants: B = F

  var handledComponents: Set[IdPath] = Set.empty

  var gumboXGen: GumboXGen = GumboXGen()

  var entrypointKinds: ISZ[ST] = ISZ()
  var entryPointHandlers: ISZ[ST]= ISZ()

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
   * AppProviderPlugin
   ******************************************************************************************/

  override def canHandleAppProviderPlugin(): B = { return T }

  override def handleAppProviderPlugin(projectDirectories: ProjectDirectories,
                                       arsitOptions: ArsitOptions,
                                       symbolTable: SymbolTable,
                                       aadlTypes: AadlTypes,
                                       reporter: Reporter): ISZ[FileResource] = {
    val runtimePath = s"${projectDirectories.architectureDir}/${arsitOptions.packageName}/runtimemonitor"

    var resources: ISZ[FileResource]= ISZ()

    val runtimepackage = s"${arsitOptions.packageName}.runtimemonitor"

    val captureKind =
      st"""// #Sireum
          |
          |package $runtimepackage
          |
          |import org.sireum._
          |import ${arsitOptions.packageName}._
          |
          |${StringTemplate.doNotEditComment()}
          |
          |@enum object CaptureKind {
          |  ${(entrypointKinds, "\n")}
          |}
          |"""
    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/CaptureKind.scala", captureKind, T, T)

    val process =
      st"""// #Sireum
          |
          |package ${runtimepackage}
          |
          |import org.sireum._
          |import ${arsitOptions.packageName}._
          |
          |${StringTemplate.doNotEditComment()}
          |
          |object GumboXDispatcher {
          |  def dispatch(captureKind: CaptureKind.Type, preContainer: Option[art.DataContent], postContainer: Option[art.DataContent]): B = {
          |    captureKind match {
          |      ${(this.entryPointHandlers, "\n")}
          |      case _ => halt("Infeasible")
          |    }
          |  }
          |}"""

    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/GumboXDispathcer.scala", process, T, F)

    return resources
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

    val containers = GumboXGenUtil.generateContainer(component, componentNames, annexInfo, aadlTypes)

    val containersPath = s"${projectDirectories.dataDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}__Containers.scala"
    resources = resources :+
      ResourceUtil.createResourceH(containersPath, containers.genContainers(), T, T)

    val preInitMethodName = s"pre_${EntryPoints.initialise.name}"
    val postInitMethodName = s"post_${EntryPoints.initialise.name}"

    val initBody =
      st"""${epCompanionName}.${preInitMethodName}()
          |
          |// implement the following method in 'component':  def ${EntryPoints.initialise.string}(api: ${componentNames.apiInitialization}): Unit = {}
          |component.${EntryPoints.initialise.string}(${ApiTemplate.apiInitializationId})
          |
          |${epCompanionName}.${postInitMethodName}()
          |
          |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""


    val preComputeMethodName = s"pre_${EntryPoints.compute.name}"
    val postComputeMethodName = s"post_${EntryPoints.compute.name}"

    // TODO:
    val computeBody: ST = {
      val s = StringUtil.split_PreserveEmptySegments(entryPointTemplate.defaultComputeBody.render, c => c == '\n')
      var newLines: ISZ[String] = ISZ()
      for(l <- s) {
        val o = ops.StringOps(l)
        if (o.contains("Art.receiveInput")) {
          newLines = newLines :+ l :+ s"\n${epCompanionName}.${preComputeMethodName}()"
        } else if (o.contains("Art.sendOutput")) {
          newLines = newLines :+ s"${epCompanionName}.${postComputeMethodName}()\n\n$l"
        } else {
          newLines = newLines :+ l
        }
      }
      st"${(newLines, "\n")}"
    }

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
/*
    val epCompanion =
      st"""@ext object ${epCompanionName} {
          |  def ${preInitMethodName}(): Unit = $$
          |  def ${postInitMethodName}(): Unit = $$
          |
          |  def ${preComputeMethodName}(): Unit = $$
          |  def ${postComputeMethodName}(): Unit = $$
          |}"""
*/
    val preContainer: String = "preStateContainer_wL"
    val postContainer: String = "postStateContainer_wL"

    /*
    val optInit: ST =
      if (this.gumboXGen.initializeEntryPointHolder.contains(component.path)) {
        val simpleCepPreContainer = GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)
        st"""val result: B = ${(simpleCepPreContainer, ".")}($postContainer)
            |println(s"${component.identifier}.initialise: Post-condition: $${if (result) "" else "un"}satisfied")"""
      } else {
        st"// checking the post-state values of ${component.identifier}'s initialise entrypoint is not required"
      }
*/

    val optInit: ST =
      if (this.gumboXGen.initializeEntryPointHolder.contains(component.path)) {
        val simpleCepPreContainer = GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)
        st"""val result: B = ${(simpleCepPreContainer, ".")}(postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_wL}])
            |println(s"${component.identifier}.initialise: Post-condition: $${if (result) "" else "un"}satisfied")
            |return result"""
      } else {
        st"""// checking the post-state values of ${component.identifier}'s initialise entrypoint is not required
            |return T"""
      }

    /*
    val optPreCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Pre.nonEmpty =>
          val simpleCepPreContainer = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPreContainer}(${preContainer}.get)
              |println(s"${component.identifier}.timeTriggered: Pre-condition: $${if (result) "" else "un"}satisfied")"""
        case _ =>
          st"// checking the pre-state values of ${component.identifier}'s compute entrypoint is not required"
      }
*/
    val optPreCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Pre.nonEmpty =>
          val simpleCepPreContainer = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPreContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_wL}])
              |println(s"${component.identifier}.timeTriggered: Pre-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the pre-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

/*
    val optPostCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Post.nonEmpty =>
          val simpleCepPostContainer = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPostContainer}(${preContainer}.get, $postContainer)
              |println(s"${component.identifier}.timeTriggered: Post-condition: $${if (result) "" else "un"}satisfied")"""
        case _ =>
          st"// checking the post-state values of ${component.identifier}'s compute entrypoint is not required"
      }
*/
    val optPostCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Post.nonEmpty =>
          val simpleCepPostContainer = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPostContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_wL}], postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_wL}])
              |println(s"${component.identifier}.timeTriggered: Post-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the post-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }


    val runtimePackage = s"${componentNames.basePackage}.runtimemonitor"

    val postInitKind = st"${runtimePackage}.CaptureKind.${componentNames.identifier}_postInit"
    val preComputeKind = st"${runtimePackage}.CaptureKind.${componentNames.identifier}_preCompute"
    val postComputeKind = st"${runtimePackage}.CaptureKind.${componentNames.identifier}_postCompute"

    entrypointKinds = entrypointKinds :+
      st""""${componentNames.identifier}_postInit"""" :+
      st""""${componentNames.identifier}_preCompute"""" :+
      st""""${componentNames.identifier}_postCompute""""

    val cases =
      st"""case $postInitKind =>
          |  $optInit
          |case $preComputeKind =>
          |  $optPreCompute
          |case $postComputeKind =>
          |  $optPostCompute
          |"""

    entryPointHandlers = entryPointHandlers :+ cases

    val epCompanionExt =
      st"""// #Sireum
          |
          |package ${componentNames.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${componentNames.basePackage}._
          |
          |${StringTemplate.doNotEditComment()}
          |
          |object ${epCompanionName} {
          |
          |  ${containers.lastDataPortVars}
          |  var $preContainer: Option[${GumboXGenUtil.genContainerName(componentNames.componentSingletonType, T, T)}] = None()
          |
          |  def ${preInitMethodName}(): Unit = {
          |    // assume/require contracts cannot refer to incoming ports or
          |    // state variables so nothing to do here
          |  }
          |
          |  def ${postInitMethodName}(): Unit = {
          |    // block the component while its post-state values are retrieved
          |    val $postContainer =
          |      ${containers.capturePostState_wL()}
          |
          |    ${componentNames.basePackage}.runtimemonitor.RuntimeMonitor.update1(${componentNames.archInstanceName}.id, $postInitKind, $postContainer)
          |
          |    // the rest of this could be done in a separate thread
          |
          |    //val json = ${containers.postStateContainerJsonFrom_wL}(postStateContainer_wL, T)
          |    //println(s"${component.identifier}.initialise: Post-State values: $$json")
          |
          |    /*
          |    $optInit
          |    */
          |  }
          |
          |  def ${preComputeMethodName}(): Unit = {
          |    // block the component while its pre-state values are retrieved
          |    $preContainer = Some(
          |      ${containers.capturePreState_wL()})
          |
          |    ${componentNames.basePackage}.runtimemonitor.RuntimeMonitor.update1(${componentNames.archInstanceName}.id, $preComputeKind, ${preContainer}.get)
          |
          |    // the rest of this could be done in a separate thread
          |    /*
          |    val json = ${containers.preStateContainerJsonFrom_wL}(preStateContainer_wL.get, T)
          |    println(s"${component.identifier}.timeTriggered: Pre-State values: $$json")
          |
          |    $optPreCompute
          |    */
          |  }
          |
          |  def ${postComputeMethodName}(): Unit = {
          |    // block the component while its post-state values are retrieved
          |    val $postContainer =
          |      ${containers.capturePostState_wL()}
          |
          |    ${componentNames.basePackage}.runtimemonitor.RuntimeMonitor.update2(${componentNames.archInstanceName}.id, $postComputeKind, ${preContainer}.get, $postContainer)
          |
          |    /*
          |    // the rest of this could be done in a separate thread
          |
          |    val json = ${containers.postStateContainerJsonFrom_wL}(postStateContainer_wL, T)
          |    println(s"${component.identifier}.timeTriggered: Post-State values: $$json")
          |
          |    $optPostCompute
          |    */
          |  }
          |}"""

    val path = s"${projectDirectories.bridgeDir}/${componentNames.packagePath}/${epCompanionName}.scala"
    val epCompanionExtR =
      ResourceUtil.createResource(path, epCompanionExt, T)

    return EntryPointProviderPlugin.EntryPointContributions(
      imports = ISZ(),
      bridgeCompanionBlocks = ISZ(),
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
