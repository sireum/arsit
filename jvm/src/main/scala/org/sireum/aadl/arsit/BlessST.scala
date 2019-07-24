// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._

object BlessST {

  val vizObjectName: String = "StateMachineVisualizer"


  @pure def main(packageName: String,
                 imports: ISZ[ST],
                 completeStateEnumName: String,
                 states: ISZ[ST],
                 componentName: String,
                 bridgeName: String,
                 initialState: String,
                 globalVars: ISZ[ST],
                 methods: ISZ[ST],
                 extensions: ISZ[ST]
                ): ST = {
    return st"""// #Sireum
               |
               |package $packageName
               |
               |import org.sireum._
               |${(imports, "\n")}
               |
               |${doNotEditComment()}
               |
               |@enum object ${completeStateEnumName} {
               |  ${(states, "\n")}
               |}
               |
               |@record class ${componentName}(api: ${bridgeName}.Api) {
               |
               |  var currentState : ${completeStateEnumName}.Type = ${completeStateEnumName}.${initialState}
               |
               |  ${if(globalVars.isEmpty) "// no global vars" else st"${(globalVars, "\n")}" }
               |
               |  ${(methods, "\n\n")}
               |
               |
               |  //methods for execution states
               |  def Finalize_Entrypoint(): Unit = {}
               |
               |  def Activate_Entrypoint(): Unit = {}
               |
               |  def Deactivate_Entrypoint(): Unit = {}
               |
               |  def Recover_Entrypoint(): Unit = {}
               |}
               |
               |${(extensions, "\n")}
               |"""
  }

  @pure def method(name: ST,
                   params: ISZ[ST],
                   body: ST,
                   retType: ST): ST = {
    return st"""def ${name}(${params}): ${retType} = {
               |  ${body}
               |}"""
  }


  @pure def externalObjectSlang(name: ST,
                                methods: ISZ[ST]) : ST = {
    return st"""@ext object ${name} {
               |  ${(methods, "\n\n")}
               |}"""
  }

  @pure def externalObjectJVM(packageName: ST,
                              name: ST,
                              imports: ISZ[ST],
                              methods: ISZ[ST]) : ST = {
    return st"""package ${packageName}
               |
               |import org.sireum._
               |${(imports, "\n")}
               |
               |object ${name} {
               |  ${(methods, "\n\n")}
               |}
               |"""
  }


  @pure def extMethod(name: ST,
                      params: ISZ[ST],
                      retType: ST): ST = {
    return st"def ${name}(${params}): ${retType} = "
  }

  @pure def ifST(ifbranch: (ST, ST), elsifs: ISZ[(ST, ST)]): ST = {
    val e = elsifs.map(x => st"""else if(${x._1}) {
                                |  ${x._2}
                                |}
                                |""")
    return st"""if(${ifbranch._1}) {
               |  ${ifbranch._2}
               |}
               |${e}"""
  }

  @pure def variableDec(varName: String,
                        varType: ST,
                        defaultValue: ST): ST = {
    return st"var ${varName}: ${varType} = ${defaultValue}"
  }

  @pure def portSend(portName: String,
                     arg: ST): ST = {
    return st"api.send${portName}(${arg})"
  }

  @pure def portFetch(portName: String): ST = {
    return st"api.get${portName}()"
  }

  @pure def portGet(portName: String): ST = {
    return st"${portFetch(portName)}.get"
  }

  @pure def portQuery(portName: String): ST = {
    // return st"ports.contains(api.${portName}_Id)"  // FIXME transpiler doesn't handle ((Z) => B @pure)
    return st"contains(dispatchedPorts, api.${portName}_Id)"
  }

  @pure def dispatchedPortsDec(): ST = {
    return st"dispatchedPorts : ISZ[art.Art.PortId]"
  }

  @pure def wrapDispatchedPorts(): ST = {
    return st"val ports = org.sireum.ops.ISZOps(dispatchedPorts)"
  }

  @pure def doNotEditComment(): ST = {
    return st"// This file was auto-generated.  Do not edit"
  }


  @pure def tranpilerWorkaroundContains(): ST = {
    return st"""def contains(ids: ISZ[art.Art.PortId], id: art.Art.PortId): B = {
               |  for (i <- ids) {
               |    if (i == id) {
               |      return T
               |    }
               |  }
               |  return F
               |}"""
  }


  @pure def debugRegister(): ST = {
    return st"art.ArtDebug.setDebugObject(api.id.string, debugObject)"
  }

  @pure def debugObjectAccess(fieldName: String): ST = {
    return st"debugObject.${fieldName}"
  }

  @pure def debugObject(componentName: String,
                        fields: ISZ[ST],
                        names: ISZ[ST]): ST = {
    val vars: ST = st"${(names.map(s => st"${s} = $${$s}"), "\n|\n|")}"
    val tq: ST = Library.tripleQuote // TODO how to write triple quote in slang
    val flds: ST = if(fields.isEmpty) st"// no global vars" else st"${(fields, ",\n")}"

    return st"""@record class ${componentName}_DebugObject (
               |  ${flds}
               |) {
               |  override def string: String = {
               |    return  st$tq$vars$tq.render
               |  }
               |}
               |"""
  }

  @pure def debugObjectDec(componentName: String,
                           initExps: ISZ[ST]): ST = {
    val inits: ST = if(initExps.isEmpty) st"// no global vars" else st"${(initExps, ",\n")}"
    return st"""val debugObject : ${componentName}_DebugObject = ${componentName}_DebugObject (
               |  ${inits}
               |)
               |"""
  }




  @pure def vizStateType(m: BTSStateCategory.Type): ST = {
    val name = m match {
      case BTSStateCategory.Execute => "EXECUTE"
      case BTSStateCategory.Complete => "COMPLETE"
      case BTSStateCategory.Final => "FINAL"
      case BTSStateCategory.Initial => "INITIAL"
    }
    return st"StateType.${name}"
  }

  @pure def vizCreate(stateName: String,
                      desc: ST,
                      stateTypes: ISZ[ST]): ST = {
    return st"""val ${stateName} = State.create("${stateName}", "${desc}", Seq(${(stateTypes, ", ")}))"""
  }

  @pure def vizBuildSm(stateDecs: ISZ[ST], id: String, label: String, initialState: String, states: ISZ[ST], trans: ISZ[ST]): ST = {
    return st"""{
               |  ${(stateDecs, "\n")}
               |
               |  addStateMachineView($id, "${label}",
               |    StateMachine
               |      .builder()
               |      .addStates(${(states, ", ")})
               |      ${(trans, "\n")}
               |      .setInitialState(${initialState})
               |      .build())
               |}"""
  }

  @pure def vizTrans(src: String, dst: String, name: String): ST = {
    return st""".addTransition(Transition.create(${src}, ${dst}, "${name}"))"""
  }


  @pure def vizPackageName(basePackage: String): ST = {
    return st"$basePackage.util"
  }

  @pure def vizSlangObject(basePackage: String): ST = {
    return st"""// #Sireum
               |
               |package ${vizPackageName(basePackage)}
               |
               |import org.sireum._
               |
               |${doNotEditComment()}
               |
               |@ext object ${vizObjectName} {
               |  def createStateMachines(): Unit = $$
               |
               |  def transition(componentId: art.Art.PortId, stateName: String): Unit = $$
               |}
               |"""
  }

  @pure def vizCallCreateStateMachines(basePackage: String): ST = {
    return st"${basePackage}.util.${vizObjectName}.createStateMachines()"
  }

  @pure def vizCallTransition(basePackage: String): ST = {
    return st"""${basePackage}.util.${vizObjectName}.transition(api.id, s"$$currentState") // TODO transpiler handle $$currentState.name"""
  }

  @pure def vizCallTransitionWithStateName(basePackage: String, stateName: String): ST = {
    return st"""${basePackage}.util.${vizObjectName}.transition(api.id, "$stateName")"""
  }

  @pure def vizExtObject(basePackage: String,
                         imports: ISZ[ST],
                         entries: ISZ[ST]
                        ): ST = {
    return st"""package ${vizPackageName(basePackage)}
               |
               |${(imports, "\n")}
               |import java.util.concurrent.atomic.AtomicBoolean
               |import ${basePackage}.Arch
               |import org.santos.inspectorgui.fsm.model.{State, StateMachine, StateType, Transition}
               |import javax.swing.SwingUtilities
               |import org.sireum.{IS, Z}
               |
               |${doNotEditComment()}
               |
               |object ${vizObjectName}_Ext {
               |
               |  private val blessVisualizer = ${vizBlessVizName()}()
               |  blessVisualizer.init()
               |
               |  var isFirst = new AtomicBoolean(true)
               |
               |  def addStateMachineView(bridgeId: Z, name: org.sireum.String, stateMachine: StateMachine): Unit =
               |    SwingUtilities.invokeAndWait(() => blessVisualizer.addStateMachineView(bridgeId, name.value, stateMachine))
               |
               |  def updateStateMachineView(bridgeId: Z, state: org.sireum.String): Unit =
               |    SwingUtilities.invokeLater(() => blessVisualizer.updateStateMachine(bridgeId, state.value))
               |
               |  def createStateMachines(): Unit = {
               |    if(isFirst.getAndSet(false)){
               |        ${(entries, "\n")}
               |    }
               |  }
               |
               |  def transition(componentId: art.Art.PortId, stateName: org.sireum.String): Unit = {
               |    updateStateMachineView(componentId, stateName.value)
               |  }
               |}
               |
               |
               |"""
  }

  @pure def vizBlessVizName(): ST = {
    return st"BlessVisualizer"
  }

  @pure def vizBlessViz(basePackage: String): ST = {
    return st"""package ${vizPackageName(basePackage)}
        |
        |import javax.swing._
        |import org.santos.inspectorgui.fsm.form.StateMachineViewPanel
        |import org.santos.inspectorgui.fsm.model.{StateMachine, Transition}
        |import org.sireum.{HashMap, Z}
        |
        |import scala.collection.JavaConverters
        |
        |object ${vizBlessVizName()} {
        |  def apply(): ${vizBlessVizName()} = new ${vizBlessVizName()}
        |}
        |
        |class ${vizBlessVizName()} {
        |
        |  var frame: JFrame = _
        |  var tabbedPane: JTabbedPane = _
        |
        |  private var fsmMap: HashMap[Z, StateMachine] = org.sireum.HashMap.empty
        |  private var panelMap: HashMap[Z, StateMachineViewPanel] = org.sireum.HashMap.empty
        |
        |  def init(): Unit = {
        |    initialize()
        |  }
        |
        |  def initialize(): Unit = {
        |    frame = new JFrame()
        |
        |    frame.setBounds(100, 10, 675, 450)
        |
        |    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        |
        |    tabbedPane = new JTabbedPane()
        |    frame.getContentPane.add(tabbedPane)
        |  }
        |
        |  def addStateMachineView(bridgeId: Z, name: String, fsm: StateMachine): Unit = {
        |    try {
        |      if (fsmMap.contains(bridgeId)) {
        |        throw new IllegalStateException("Cannot add duplicate bridge view to ${vizBlessVizName()}.")
        |      }
        |      fsmMap = fsmMap + (bridgeId, fsm)
        |      val panel = new StateMachineViewPanel(fsm)
        |      panelMap = panelMap + (bridgeId, panel)
        |      tabbedPane.addTab(name, panel)
        |      frame.pack()
        |      frame.setVisible(true)
        |    } catch {
        |      case t: Throwable => t.printStackTrace()
        |    }
        |  }
        |
        |  def updateStateMachine(bridgeId: Z, state: String): Unit = {
        |    try {
        |      val fsm = fsmMap.get(bridgeId).get
        |      val transition = findTransitionWithStateNames(state, fsm)
        |      var s = ""
        |      if (!art.ArtDebug.getDebugObject[Any](bridgeId.string).isEmpty) {
        |        s = art.ArtDebug.getDebugObject[Any](bridgeId.string).get.toString
        |      }
        |
        |      transition match {
        |        case scala.Some(t) => fsm.update(t, s)
        |        case scala.None => panelMap.get(bridgeId).get.beginTimeline() // should occur once if graph made correctly
        |      }
        |    } catch {
        |      case t: Throwable => t.printStackTrace()
        |    }
        |  }
        |
        |  private def findTransitionWithStateNames(state: String, fsm: StateMachine): scala.Option[Transition] =
        |    toIterator(fsm.getGraph.getOutEdges(fsm.currentState)).find(_.getTo.getName.equals(state))
        |
        |  private def toIterator[T](l: java.util.Collection[T]): Iterator[T] = {
        |    if (l == null) {
        |      return Iterator[T]()
        |    }
        |    JavaConverters.collectionAsScalaIterableConverter(l).asScala.toIterator
        |  }
        |}
        |"""
  }
}