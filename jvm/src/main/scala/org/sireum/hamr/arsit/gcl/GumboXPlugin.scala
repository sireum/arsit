// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.plugin.{BehaviorEntryPointProviderPlugin, EntryPointProviderPlugin, PlatformProviderPlugin}
import org.sireum.hamr.arsit.templates.{ApiTemplate, EntryPointTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause
import org.sireum.message.Reporter

@record class GumboXPlugin
  extends EntryPointProviderPlugin with BehaviorEntryPointProviderPlugin with PlatformProviderPlugin {

  val name: String = "GumboX Plugin"

  var processedDatatypeInvariants: B = F

  var handledComponents: Set[IdPath] = Set.empty

  var gumboXGen: GumboXGen = GumboXGen()

  var entrypointKinds: ISZ[ST] = ISZ()
  var entryPointHandlers: ISZ[ST] = ISZ()

  var genTestCases: ISZ[ST] = ISZ()
  var testSuiteCases: ISZ[ST] = ISZ()
  var testSuiteCaseIds: ISZ[ST] = ISZ()

  var containerMap: Map[IdPath, GumboXGenUtil.Container] = Map.empty

  def getContainer(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): GumboXGenUtil.Container = {
    return containerMap.getOrElse(component.path, GumboXGenUtil.generateContainer(component, componentNames, annexInfo, aadlTypes))
  }

  var modelInfo: ISZ[ST] = ISZ()

  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes): B = {

    if (aadlTypes.rawConnections) {
      return F
    }

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

    return (hasDatatypeInvariants || componentHasGumboSubclauseInfo)
  }

  /** Common method for entrypoint and behavior provider plugin -- i.e. only needs to be called once
    * per component
    */
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
   * PlatformProviderPlugin
   ******************************************************************************************/

  val enableRuntimeMonitoring: String = "enableRuntimeMonitoring"

  def runtimeMonitoringEnabled(arsitOptions: ArsitOptions): B = {
    return ops.ISZOps(arsitOptions.experimentalOptions).contains(enableRuntimeMonitoring)
  }

  override def canHandlePlatformProviderPlugin(arsitOptions: ArsitOptions,
                                               symbolTable: SymbolTable,
                                               aadlTypes: AadlTypes): B = {
    return runtimeMonitoringEnabled(arsitOptions)
  }

  override def handlePlatformProviderPlugin(projectDirectories: ProjectDirectories,
                                            arsitOptions: ArsitOptions,
                                            symbolTable: SymbolTable,
                                            aadlTypes: AadlTypes,
                                            reporter: Reporter): ISZ[PlatformProviderPlugin.PlatformContributions] = {
    val runtimePath = s"${projectDirectories.architectureDir}/${arsitOptions.packageName}/runtimemonitor"

    var resources: ISZ[FileResource] = ISZ()

    val runtimePackage = s"${arsitOptions.packageName}.runtimemonitor"

    val observationKind =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${arsitOptions.packageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@enum object ObservationKind {
          |  ${(entrypointKinds, "\n")}
          |}
          |"""
    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/ObservationKind.scala", observationKind, T, T)

    val process =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${arsitOptions.packageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object GumboXDispatcher {
          |  def checkContract(observationKind: ObservationKind.Type, preContainer: Option[art.DataContent], postContainer: Option[art.DataContent]): B = {
          |    observationKind match {
          |      ${(entryPointHandlers, "\n")}
          |      case _ => halt("Infeasible")
          |    }
          |  }
          |
          |  def genTestSuite(testCases: ISZ[(Z, ISZ[ST])]): Unit = {
          |    val tq = ${DSCTemplate.tqq}
          |
          |    val testRoot = Os.path(".") / "src" / "test" / "bridge"
          |
          |    ${(testSuiteCaseIds, "\n")}
          |
          |    def genUniqueSuiteName(path: Os.Path, prefix: String): String = {
          |      var i = 0
          |      while(true) {
          |        val cand = path / s"$${prefix}_$${i}.scala"
          |        if (!cand.exists) {
          |          return s"$${prefix}_$${i}"
          |        }
          |        i = i + 1
          |      }
          |      halt("Infeasible")
          |    }
          |
          |    for (p <- testCases) {
          |      art.Art.BridgeId.fromZ(p._1) match {
          |        ${(testSuiteCases, "\n")}
          |        case x => halt(s"Infeasible bridge id: $$x")
          |      }
          |    }
          |  }
          |
          |  def genTestCase(observationKind: ObservationKind.Type, preContainer: Option[String], postContainer: Option[String], testNameSuffix: Option[String]): ST = {
          |    val tq = ${DSCTemplate.tqq}
          |    val suffix: String =
          |      if (testNameSuffix.nonEmpty) testNameSuffix.get
          |      else ""
          |
          |    observationKind match {
          |      ${(genTestCases, "\n")}
          |      case _ => return st"// TODO $${observationKind}"
          |    }
          |  }
          |}"""

    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/GumboXDispatcher.scala", process, T, F)

    val platformSetupBlocks = ISZ(st"${runtimePackage}.RuntimeMonitor.init()")

    val runtimeMonitor: ST =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import art.Art.BridgeId
          |
          |@ext object RuntimeMonitor {
          |
          |  def init(): Unit = $$
          |
          |  def observePreState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = $$
          |
          |  def observePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = $$
          |
          |  def observePrePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = $$
          |}
          |"""
    val rmpath = s"${runtimePath}/RuntimeMonitor.scala"
    resources = resources :+ ResourceUtil.createResource(rmpath, runtimeMonitor, T)

    val runtimeMonitorExt: ST =
      st"""package $runtimePackage
          |
          |import org.sireum._
          |import art.Art._
          |
          |object RuntimeMonitor_Ext {
          |
          |  var gui: GUI = _
          |
          |  def init(): Unit = {
          |    gui = new GUI()
          |    gui.init()
          |  }
          |
          |  def observePreState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = {
          |    gui.observePreState(bridgeId, observationKind, pre)
          |  }
          |
          |  def observePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = {
          |    gui.observePostState(bridgeId, observationKind, post)
          |  }
          |
          |  def observePrePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = {
          |    gui.observePrePostState(bridgeId, observationKind, pre, post)
          |  }
          |}"""
    val rmpathExt = s"${runtimePath}/RuntimeMonitor_Ext.scala"
    resources = resources :+ ResourceUtil.createResource(rmpathExt, runtimeMonitorExt, T)

    val gui: ST =
      st"""package $runtimePackage
          |
          |import org.sireum._
          |import art.Art.BridgeId
          |import ${arsitOptions.packageName}.JSON
          |
          |import java.awt.{BorderLayout, Dimension}
          |import javax.swing._
          |import javax.swing.table.AbstractTableModel
          |
          |class GUI extends JFrame {
          |
          |  var jtable: JTable = _
          |  var model: TableModel = _
          |
          |  def init(): Unit = {
          |    this.setTitle("Visualizer")
          |
          |    model = new TableModel()
          |    jtable = new JTable()
          |    jtable.setModel(model)
          |
          |    add(jtable.getTableHeader(), BorderLayout.PAGE_START)
          |    add(jtable, BorderLayout.CENTER)
          |
          |    setPreferredSize(new Dimension(500, 300))
          |    pack()
          |    setResizable(true)
          |    setLocationRelativeTo(null)
          |    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
          |    setVisible(true)
          |  }
          |
          |  def observePreState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = {
          |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, pre, None()))
          |  }
          |
          |  def observePostState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = {
          |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, None(), Some(post)))
          |  }
          |
          |  def observePrePostState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = {
          |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, pre, Some(post)))
          |  }
          |
          |  def dispatch(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: Option[art.DataContent]): Unit = {
          |    model.addRow(Row(bridge, observationKind,
          |      GumboXDispatcher.checkContract(observationKind, pre, post),
          |      if (pre.nonEmpty) Some(JSON.from_artDataContent(pre.get, T)) else None(),
          |      if (post.nonEmpty) Some(JSON.from_artDataContent(post.get, T)) else None()))
          |  }
          |}
          |
          |case class Row(bridgeId: BridgeId, observationKind: ObservationKind.Type, result: Boolean, pre: Option[String], post: Option[String])
          |
          |class TableModel extends AbstractTableModel {
          |  val columnNames = Array("BridgeId", "Kind", "Satisified")
          |
          |  var data: ISZ[Row] = ISZ()
          |
          |  def addRow(row: Row): Unit = {
          |    data = data :+ row
          |    fireTableRowsInserted(data.size.toInt - 1, data.size.toInt - 1)
          |  }
          |
          |  override def getRowCount: Int = {
          |    return data.size.toInt
          |  }
          |
          |  override def getColumnCount: Int = {
          |    return columnNames.length
          |  }
          |
          |  override def getColumnName(column: Int): java.lang.String = {
          |    return columnNames(column)
          |  }
          |
          |  override def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
          |    return columnIndex match {
          |      case 0 => data(rowIndex).bridgeId.string.native
          |      case 1 => data(rowIndex).observationKind.string.native
          |      case 2 => data(rowIndex).result.string.native
          |      case _ => halt("Infeasible")
          |    }
          |  }
          |}
          |"""
    val guipath = s"${runtimePath}/GUI.scala"
    resources = resources :+ ResourceUtil.createResource(guipath, gui, T)

    return ISZ(PlatformProviderPlugin.PlatformSetupContributions(imports = ISZ(), blocks = platformSetupBlocks, resources = resources))
  }

  /******************************************************************************************
   * EntryPoint provider
   ******************************************************************************************/

  override def canHandleEntryPointProvider(component: AadlThreadOrDevice,
                                           resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                           arsitOptions: ArsitOptions,
                                           symbolTable: SymbolTable,
                                           aadlTypes: AadlTypes): B = {
    return runtimeMonitoringEnabled(arsitOptions) &&
      !handledComponents.contains(component.path) &&
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
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
    var hasStateVariables: B = F

    val annexInfo: Option[(GclSubclause, GclSymbolTable)] = resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex_, gclSymbolTable_)) =>
        hasStateVariables = annex_.state.nonEmpty
        Some((annex_, gclSymbolTable_))
      case _ => None()
    }


    val containers = getContainer(component, componentNames, annexInfo, aadlTypes)

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
      for (l <- s) {
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

    val preContainer: String = "preStateContainer_wL"
    val postContainer: String = "postStateContainer_wL"

    val optInit: ST =
      if (this.gumboXGen.initializeEntryPointHolder.contains(component.path)) {
        val simpleCepPreContainer = GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)
        st"""val result: B = ${(simpleCepPreContainer, ".")}(postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
            |println(s"${component.identifier}.initialise: Post-condition: $${if (result) "" else "un"}satisfied")
            |return result"""
      } else {
        st"""// checking the post-state values of ${component.identifier}'s initialise entrypoint is not required
            |return T"""
      }

    val optPreCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Pre.nonEmpty =>
          val simpleCepPreContainer = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPreContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}])
              |println(s"${component.identifier}.timeTriggered: Pre-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the pre-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

    val optPostCompute: ST =
      this.gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Post.nonEmpty =>
          val simpleCepPostContainer = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPostContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}], postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
              |println(s"${component.identifier}.timeTriggered: Post-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the post-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }


    val runtimePackage = s"${componentNames.basePackage}.runtimemonitor"

    val postInitKind = st"${componentNames.identifier}_postInit"
    val preComputeKind = st"${componentNames.identifier}_preCompute"
    val postComputeKind = st"${componentNames.identifier}_postCompute"

    val postInitKindFQ = st"${runtimePackage}.ObservationKind.${postInitKind}"
    val preComputeKindFQ = st"${runtimePackage}.ObservationKind.${preComputeKind}"
    val postComputeKindFQ = st"${runtimePackage}.ObservationKind.${postComputeKind}"

    entrypointKinds = entrypointKinds :+
      st""""${componentNames.identifier}_postInit"""" :+
      st""""${componentNames.identifier}_preCompute"""" :+
      st""""${componentNames.identifier}_postCompute""""

    val entryPointCases =
      st"""case $postInitKindFQ =>
          |  $optInit
          |case $preComputeKindFQ =>
          |  $optPreCompute
          |case $postComputeKindFQ =>
          |  $optPostCompute
          |"""

    entryPointHandlers = entryPointHandlers :+ entryPointCases

    var postInitCases: ISZ[ST] = ISZ()
    var preComputeCases: ISZ[ST] = ISZ()
    var postComputeCases: ISZ[ST] = ISZ()

    this.gumboXGen.initializeEntryPointHolder.get(component.path) match {
      case Some(holder) =>
        val simple_IEP_Post_container = st"${(GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames), ".")}"
        postInitCases = postInitCases :+
          st"""|test(s"${postInitKind}: Check Post-condition$$suffix") {
               ||  val postJson: String = st$${tq}$${postContainer.get}$${tq}.render
               ||  val postContainer = ${componentNames.basePackage}.${containers.postStateContainerJsonTo_PS}(postJson).left
               ||  assert(${simple_IEP_Post_container}(postContainer))
               ||}"""
      case _ =>
    }

    this.gumboXGen.computeEntryPointHolder.get(component.path) match {
      case Some(holder) =>
        if (holder.CEP_Pre.nonEmpty) {
          val simple_CEP_Pre_container = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          def gen(kind: ST): ST = {
            return(st"""|test(s"$kind: Check Pre-condition$$suffix") {
                        ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                        ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                        ||  assert(${simple_CEP_Pre_container}(preContainer))
                        ||}""")
          }

          preComputeCases = preComputeCases :+ gen(preComputeKind)
          postComputeCases = postComputeCases :+ gen(postComputeKind)
        }

        if (holder.CEP_Post.nonEmpty) {
          val simple_CEP_Post_container = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          postComputeCases = postComputeCases :+
            st"""|test(s"${postComputeKind}: Check Post-condition$$suffix") {
                 ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                 ||  val postJson: String = st$${tq}$${postContainer.get}$${tq}.render
                 ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                 ||  val postContainer = ${componentNames.basePackage}.${containers.postStateContainerJsonTo_PS}(postJson).left
                 ||  assert(${simple_CEP_Post_container}(preContainer, postContainer))
                 ||}"""
        }

        // FIXME: add sporadic cb testing support
        if (component.isPeriodic()) {
          val methodToCall: String = if (hasStateVariables) "testComputeCB_wLV" else "testComputeCBV"
          def gen2(kind: ST): ST = {
            return(st"""|test(s"$kind: Run $methodToCall$$suffix") {
                        ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                        ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                        ||  println($methodToCall(preContainer))
                        ||}""")
          }

          preComputeCases = preComputeCases :+ gen2(preComputeKind)
          postComputeCases = postComputeCases :+ gen2(postComputeKind)
        }

      case _ =>
    }

    def trans(kindFQ: ST, kind: ST, cases: ISZ[ST]): Option[ST] = {
      return (
        if (cases.nonEmpty)
          Some(
            st"""case $kindFQ =>
                |  return (st${DSCTemplate.tq}// Begin test cases for ${kind}
                |              |
                |              ${(cases, "\n|\n")}
                |              |// End test cases for ${kind}${DSCTemplate.tq})""")
        else None()
        )
    }

    genTestCases = genTestCases :+
      st"""${trans(postInitKindFQ, postInitKind, postInitCases)}
          |${trans(preComputeKindFQ, preComputeKind, preComputeCases)}
          |${trans(postComputeKindFQ, postComputeKind, postComputeCases)}"""

    val cid = st"${componentNames.componentSingletonType}_id"
    testSuiteCaseIds = testSuiteCaseIds :+ st"val ${cid} = ${componentNames.archInstanceName}.id"

    val baseName: String =
      if (component.isPeriodic()) ops.ISZOps(GumboXGen.createScalaTestGumboXClassName(componentNames)).last
      else componentNames.testScalaTestName

    val testSuiteCase: ST = {
      st"""case ${cid} =>
          |  val prefix = "${componentNames.componentSingletonType}_RM_TestSuite"
          |  val path = testRoot /+ ISZ(${(for (pn <- componentNames.packageNameI) yield st""""$pn"""", ",")})
          |  val suiteName = genUniqueSuiteName(path, prefix)
          |
          |  val testSuite =
          |    st${DSCTemplate.tq}package ${componentNames.packageName}
          |        |
          |        |import org.sireum._
          |        |import ${componentNames.packageName}._
          |        |
          |        |class $${suiteName} extends ${baseName} {
          |        |  val verbose: B = true
          |        |
          |        |  var i = 0 // ensures generated test case names are unique
          |        |  def incrementI: Int = {
          |        |    i += 1
          |        |    return i
          |        |  }
          |        |
          |        |  $${(p._2, "\nincrementI\n\n")}
          |        |}${DSCTemplate.tq}
          |  val filename = path / s"$${suiteName}.scala"
          |  filename.writeOver(testSuite.render)
          |  println(s"Wrote: $${filename.toUri}")"""
    }

    testSuiteCases = testSuiteCases :+ testSuiteCase

    val epCompanionExt =
      st"""// #Sireum
          |
          |package ${componentNames.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${componentNames.basePackage}._
          |
          |${CommentTemplate.doNotEditComment_scala}
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
          |      ${containers.observePostState_wL()}
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observePostState(${componentNames.archInstanceName}.id, $postInitKindFQ, $postContainer)
          |  }
          |
          |  def ${preComputeMethodName}(): Unit = {
          |    // block the component while its pre-state values are retrieved
          |    $preContainer = Some(
          |      ${containers.observePreState_wL()})
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observePreState(${componentNames.archInstanceName}.id, $preComputeKindFQ, ${preContainer}.asInstanceOf[Option[art.DataContent]])
          |  }
          |
          |  def ${postComputeMethodName}(): Unit = {
          |    // block the component while its post-state values are retrieved
          |    val $postContainer =
          |      ${containers.observePostState_wL()}
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observePrePostState(${componentNames.archInstanceName}.id, $postComputeKindFQ, ${preContainer}.asInstanceOf[Option[art.DataContent]], $postContainer)
          |  }
          |}"""

    val path = s"${projectDirectories.bridgeDir}/${componentNames.packagePath}/${epCompanionName}.scala"
    resources = resources :+ ResourceUtil.createResource(path, epCompanionExt, T)

    return EntryPointProviderPlugin.EntryPointContributions(
      imports = ISZ(),
      bridgeCompanionBlocks = ISZ(),
      entryPoint = gEntryPoint,
      resources = resources
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
    return !handledComponents.contains(component.path) &&
      canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
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

  override def canFinaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                                     resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                     arsitOptions: ArsitOptions,
                                                     symbolTable: SymbolTable,
                                                     aadlTypes: AadlTypes): B = {
    return canHandle(component, resolvedAnnexSubclauses, symbolTable, aadlTypes)
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

    val containers = getContainer(component, componentNames, annexInfo, aadlTypes)
    val containersPath = s"${projectDirectories.dataDir}/${componentNames.packagePath}/${componentNames.componentSingletonType}__Containers.scala"
    val containersR = ResourceUtil.createResourceH(containersPath, containers.genContainers(), T, T)

    return Some(gumbox(resources = gumbox.resources ++ testHarness.resources :+ containersR))
  }

}
