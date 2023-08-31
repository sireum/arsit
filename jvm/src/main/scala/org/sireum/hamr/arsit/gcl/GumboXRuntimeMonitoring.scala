// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil.GGPortParam
import org.sireum.hamr.arsit.plugin.{EntryPointProviderPlugin, PlatformProviderPlugin}
import org.sireum.hamr.arsit.templates.{ApiTemplate, EntryPointTemplate}
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause

object GumboXRuntimeMonitoring {

  // a 'hand-me-down' container to allow the entry point plugin to communicate results
  // to the platform provider plugin
  @record class RM_Container(var entrypointKinds: ISZ[ST],
                             var entryPointHandlers: ISZ[ST],
                             var componentModelInfos: ISZ[(String, ST)],
                             var genTestCases: ISZ[ST],
                             var testSuiteCases: ISZ[ST],
                             var testSuiteCaseIds: ISZ[ST],
                             var postInitCatUpdates: ISZ[ST],
                             var preComputeCatUpdates: ISZ[ST],
                             var postComputeCatUpdates: ISZ[ST])

  def handleEntryPointProvider(component: AadlThreadOrDevice,
                               componentNames: NameProvider,

                               entryPointTemplate: EntryPointTemplate,

                               gumboXGen: GumboXGen,
                               containers: GumboXGenUtil.Container,

                               hasStateVariables: B,
                               annexInfo: Option[(GclSubclause, GclSymbolTable)],

                               rmContainer: RM_Container,

                               aadlTypes: AadlTypes,
                               projectDirectories: ProjectDirectories
                              ): EntryPointProviderPlugin.EntryPointContributions = {

    val epCompanionName: String = s"${componentNames.componentSingletonType}_EntryPoint_Companion"

    var companionBlocks: ISZ[ST] = ISZ()
    var companionExtBlocks: ISZ[ST] = ISZ()
    var resources: ISZ[FileResource] = ISZ()

    val componentMI = GumboXRuntimeMonitoring.getComponentModelInfo(component, componentNames, annexInfo, aadlTypes)
    rmContainer.componentModelInfos = rmContainer.componentModelInfos :+ componentMI

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


    val runtimePackage = s"${componentNames.basePackage}.runtimemonitor"

    val postInitKind = st"${componentNames.identifier}_postInit"
    val preComputeKind = st"${componentNames.identifier}_preCompute"
    val postComputeKind = st"${componentNames.identifier}_postCompute"

    val postInitKindFQ = st"${runtimePackage}.ObservationKind.${postInitKind}"
    val preComputeKindFQ = st"${runtimePackage}.ObservationKind.${preComputeKind}"
    val postComputeKindFQ = st"${runtimePackage}.ObservationKind.${postComputeKind}"

    val optInit: ST =
      if (gumboXGen.initializeEntryPointHolder.contains(component.path)) {

        val stateVars: ISZ[ST] = for (sv <- containers.outStateVars) yield st"""updates = updates + "${sv.originName}" ~> postContainer.${sv.name}.string"""
        var outPorts: ISZ[ST] = ISZ()
        for (o <- containers.outPorts) {
          val p = o.asInstanceOf[GGPortParam]
          if (p.isEvent) {
            outPorts = outPorts :+
              st"""if (postContainer.${p.name}.nonEmpty) {
                  |  updates = updates + "${p.originName}" ~> postContainer.${p.name}.get.string
                  |}"""
          } else {
            outPorts = outPorts :+
              st"""updates = updates + "${p.originName}" ~> postContainer.${p.name}.string"""
          }
        }

        rmContainer.postInitCatUpdates = rmContainer.postInitCatUpdates :+
          st"""case $postInitKindFQ =>
              |  var updates: Map[String, String] = Map.empty
              |  val postContainer = container.asInstanceOf[${containers.fqPostStateContainerName_PS}]
              |  ${(stateVars ++ outPorts, "\n")}
              |  return updates"""


        val simpleCepPreContainer = GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)
        st"""val result: B = ${(simpleCepPreContainer, ".")}(postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
            |println(s"${component.identifier}.initialise: Post-condition: $${if (result) "" else "un"}satisfied")
            |return result"""
      } else {
        st"""// checking the post-state values of ${component.identifier}'s initialise entrypoint is not required
            |return T"""
      }

    val optPreCompute: ST =
      gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Pre.nonEmpty =>


          //val stateVars: ISZ[ST] = for (sv <- containers.outStateVars) yield st"""updates = updates + "${sv.originName}" ~> preContainer.${sv.name}.string"""
          var inPorts: ISZ[ST] = ISZ()
          for (o <- containers.inPorts) {
            val p = o.asInstanceOf[GGPortParam]
            if (p.isEvent) {
              inPorts = inPorts :+
                st"""if (preContainer.${p.name}.nonEmpty) {
                    |  updates = updates + "${p.originName}" ~> preContainer.${p.name}.get.string
                    |}"""
            } else {
              inPorts = inPorts :+
                st"""updates = updates + "${p.originName}" ~> preContainer.${p.name}.string"""
            }
          }
          rmContainer.preComputeCatUpdates = rmContainer.preComputeCatUpdates :+
            st"""case $preComputeKindFQ =>
                |  var updates: Map[String, String] = Map.empty
                |  val preContainer = container.asInstanceOf[${containers.fqPreStateContainerName_PS}]
                |  ${(inPorts, "\n")}
                |  return updates"""


          val simpleCepPreContainer = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPreContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}])
              |println(s"${component.identifier}.timeTriggered: Pre-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the pre-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

    val optPostCompute: ST =
      gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Post.nonEmpty =>

          val stateVars: ISZ[ST] = for (sv <- containers.outStateVars) yield st"""updates = updates + "${sv.originName}" ~> postContainer.${sv.name}.string"""
          var outPorts: ISZ[ST] = ISZ()
          for (o <- containers.outPorts) {
            val p = o.asInstanceOf[GGPortParam]
            if (p.isEvent) {
              outPorts = outPorts :+
                st"""if (postContainer.${p.name}.nonEmpty) {
                    |  updates = updates + "${p.originName}" ~> postContainer.${p.name}.get.string
                    |}"""
            } else {
              outPorts = outPorts :+
                st"""updates = updates + "${p.originName}" ~> postContainer.${p.name}.string"""
            }
          }
          rmContainer.postComputeCatUpdates = rmContainer.postComputeCatUpdates :+
            st"""case $postComputeKindFQ =>
                |  var updates: Map[String, String] = Map.empty
                |  val postContainer = container.asInstanceOf[${containers.fqPostStateContainerName_PS}]
                |  ${(stateVars ++ outPorts, "\n")}
                |  return updates"""


          val simpleCepPostContainer = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPostContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}], postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
              |println(s"${component.identifier}.timeTriggered: Post-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the post-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

    rmContainer.entrypointKinds = rmContainer.entrypointKinds :+
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

    rmContainer.entryPointHandlers = rmContainer.entryPointHandlers :+ entryPointCases

    var postInitCases: ISZ[ST] = ISZ()
    var preComputeCases: ISZ[ST] = ISZ()
    var postComputeCases: ISZ[ST] = ISZ()

    gumboXGen.initializeEntryPointHolder.get(component.path) match {
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

    gumboXGen.computeEntryPointHolder.get(component.path) match {
      case Some(holder) =>
        if (holder.CEP_Pre.nonEmpty) {
          val simple_CEP_Pre_container = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"

          def gen(kind: ST): ST = {
            return (st"""|test(s"$kind: Check Pre-condition$$suffix") {
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
            return (st"""|test(s"$kind: Run $methodToCall$$suffix") {
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

    rmContainer.genTestCases = rmContainer.genTestCases :+
      st"""${trans(postInitKindFQ, postInitKind, postInitCases)}
          |${trans(preComputeKindFQ, preComputeKind, preComputeCases)}
          |${trans(postComputeKindFQ, postComputeKind, postComputeCases)}"""

    val cid = st"${componentNames.componentSingletonType}_id"
    rmContainer.testSuiteCaseIds = rmContainer.testSuiteCaseIds :+ st"val ${cid} = ${componentNames.archInstanceName}.id"

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

    rmContainer.testSuiteCases = rmContainer.testSuiteCases :+ testSuiteCase

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

  def handlePlatformProviderPlugin(rmContainer: RM_Container,
                                   basePackageName: String,
                                   projectDirectories: ProjectDirectories): PlatformProviderPlugin.PlatformContributions = {
    val runtimePath = s"${projectDirectories.architectureDir}/${basePackageName}/runtimemonitor"

    var resources: ISZ[FileResource] = ISZ()

    val runtimePackage = s"${basePackageName}.runtimemonitor"

    val observationKind =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${basePackageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@enum object ObservationKind {
          |  ${(rmContainer.entrypointKinds, "\n")}
          |}
          |"""
    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/ObservationKind.scala", observationKind, T, T)

    val process =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${basePackageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object GumboXDispatcher {
          |  def checkContract(observationKind: ObservationKind.Type, preContainer: Option[art.DataContent], postContainer: Option[art.DataContent]): B = {
          |    observationKind match {
          |      ${(rmContainer.entryPointHandlers, "\n")}
          |      case _ => halt("Infeasible")
          |    }
          |  }
          |
          |  def genTestSuite(testCases: ISZ[(Z, ISZ[ST])]): Unit = {
          |    val tq = ${DSCTemplate.tqq}
          |
          |    val testRoot = Os.path(".") / "src" / "test" / "bridge"
          |
          |    ${(rmContainer.testSuiteCaseIds, "\n")}
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
          |        ${(rmContainer.testSuiteCases, "\n")}
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
          |      ${(rmContainer.genTestCases, "\n")}
          |      case _ => return st"// TODO $${observationKind}"
          |    }
          |  }
          |
          |  def getUpdates(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, container: art.DataContent): Map[String, String] = {
          |    observationKind match {
          |      ${(rmContainer.postInitCatUpdates, "\n")}
          |      ${(rmContainer.preComputeCatUpdates, "\n")}
          |      ${(rmContainer.postComputeCatUpdates, "\n")}
          |      case _ => return Map.empty
          |    }
          |  }
          |}"""

    resources = resources :+ ResourceUtil.createResource(s"${runtimePath}/GumboXDispatcher.scala", process, T)

    val modelInfo = GumboXRuntimeMonitoring.genModelInfo(rmContainer.componentModelInfos, basePackageName)
    resources = resources :+ ResourceUtil.createResource(s"${runtimePath}/ModelInfo.scala", modelInfo, T)

    val platformSetupBlocks = ISZ(st"${runtimePackage}.RuntimeMonitor.init(${runtimePackage}.ModelInfo.modelInfo)")

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
          |  def init(modelInfo: ModelInfo): Unit = $$
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
          |  def init(modelInfo: ModelInfo): Unit = {
          |    gui = new GUI()
          |    gui.init(modelInfo)
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
          |import ${basePackageName}.JSON
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

    return PlatformProviderPlugin.PlatformSetupContributions(imports = ISZ(), blocks = platformSetupBlocks, resources = resources)
  }

  def genModelInfo(componentInfos: ISZ[(String, ST)], basePackage: String): ST = {
    val ret =
      st"""// #Sireum
          |package tc.runtimemonitor
          |
          |import org.sireum._
          |import ${basePackage}._
          |
          |object ModelInfo {
          |  ${(for (i <- componentInfos) yield i._2, "\n\n")}
          |
          |  val modelInfo: ModelInfo =
          |    ModelInfo(ISZ(
          |     ${(for (i <- componentInfos) yield i._1, ",\n")}))
          |}
          |
          |@datatype class ModelInfo(val components: ISZ[Component])
          |
          |@datatype class Component(val name: String,
          |                          val id: Z,
          |                          val dispatchProtocol: iDispatchProtocol.Type,
          |                          val state: ISZ[StateElement])
          |
          |@enum object iDispatchProtocol {
          |  "Sporadic"
          |  "Periodic"
          |}
          |
          |@enum object StateDirection {
          |  "In"
          |  "Out"
          |}
          |
          |@sig trait StateElement {
          |  def name: String
          |
          |  def slangType: String
          |
          |  def direction: StateDirection.Type
          |}
          |
          |@enum object PortKind {
          |  "Data"
          |  "Event"
          |  "EventData"
          |}
          |
          |@datatype class Port(val name: String,
          |                     val id: Z,
          |                     val kind: PortKind.Type,
          |                     val direction: StateDirection.Type,
          |                     val slangType: String) extends StateElement
          |
          |@datatype class StateVariable(val name: String,
          |                              val direction: StateDirection.Type,
          |                              val slangType: String) extends StateElement
          |"""
    return ret
  }


  @strictpure def getComponentModelInfoName(componentNames: NameProvider): String =
    s"${componentNames.componentSingletonType}_MI"

  def getComponentModelInfo(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): (String, ST) = {

    val stateVars = GumboXGenUtil.stateVarsToParams(componentNames, annexInfo, F, aadlTypes)
    val inPorts = GumboXGenUtil.inPortsToParams(component, componentNames)
    val outPorts = GumboXGenUtil.outPortsToParams(component, componentNames)

    var stateElements = ISZ[ST]()
    for (sv <- stateVars) {
      stateElements = stateElements :+
        st"""StateVariable(
            |  name = "${sv.originName}",
            |  direction = StateDirection.Out,
            |  slangType = "${sv.aadlType.nameProvider.qualifiedReferencedTypeName}")"""
    }
    for (f <- component.features if f.isInstanceOf[AadlPort]) {
      val (kind, direction, slangType): (String, String, String) = f match {
        case i: AadlEventPort => ("PortKind.Event", s"StateDirection.${i.direction.name}", "Option[Empty]")
        case i: AadlEventDataPort => ("PortKind.EventData", s"StateDirection.${i.direction.name}", i.aadlType.nameProvider.qualifiedReferencedTypeName)
        case i: AadlDataPort => ("PortKind.Data", s"StateDirection.${i.direction.name}", i.aadlType.nameProvider.qualifiedReferencedTypeName)
      }
      stateElements = stateElements :+
        st"""Port(
            |  name = "${f.identifier}",
            |  id = ${componentNames.archInstanceName}.${f.identifier}.id.toZ,
            |  kind = $kind,
            |  direction = ${direction},
            |  slangType = "")"""
    }

    val cmiName = getComponentModelInfoName(componentNames)
    val ret =
      st"""val $cmiName : Component =
          |  Component(
          |    name = "${componentNames.componentSingletonType}",
          |    id = ${componentNames.archInstanceName}.id.toZ,
          |    dispatchProtocol = iDispatchProtocol.${if (component.isPeriodic()) "Periodic" else "Sporadic"},
          |    state = ISZ(
          |      ${(stateElements, ",\n")}))"""

    return (cmiName, ret)
  }
}
