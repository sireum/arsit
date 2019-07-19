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
               |@enum object ${completeStateEnumName} {
               |  ${(states, "\n")}
               |}
               |
               |@record class ${componentName}(api: ${bridgeName}.Api) {
               |
               |  var currentState : ${completeStateEnumName}.Type = ${completeStateEnumName}.${initialState}
               |
               |  ${if(globalVars.isEmpty) "// no local vars" else st"${(globalVars, "\n")}" }
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
    return st"ports.contains(api.${portName}_Id)"
  }

  @pure def dispatchedPortsDec(): ST = {
    return st"dispatchedPorts : ISZ[art.Art.PortId]"
  }

  @pure def wrapDispatchedPorts(): ST = {
    return st"val ports = org.sireum.ops.ISZOps(dispatchedPorts)"
  }




  @pure def vizCreate(stateName: String, desc: ST): ST = {
    return st"""val ${stateName} = State.create("${stateName}", "${desc}")"""
  }

  @pure def vizBuildSm(stateDecs: ISZ[ST], id: String, label: String, initialState: String, states: ISZ[ST], trans: ISZ[ST]): ST = {
    return st"""{
               |  ${(stateDecs, "\n")}
               |
               |  Inspector.addStateMachineView($id, "${label}",
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
    return st"""${basePackage}.util.${vizObjectName}.transition(api.id, currentState.name)"""
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
               |import ${basePackage}.Inspector
               |import org.santos.inspectorgui.fsm.model.{State, StateMachine, Transition}
               |
               |object ${vizObjectName}_Ext {
               |
               |  var isFirst = new AtomicBoolean(true)
               |
               |  def createStateMachines(): Unit = {
               |    if(isFirst.getAndSet(false)){
               |        ${(entries, "\n")}
               |    }
               |  }
               |
               |  def transition(componentId: art.Art.PortId, stateName: org.sireum.String): Unit = {
               |    Inspector.updateStateMachineView(componentId, stateName.value)
               |  }
               |}
               |
               |
               |"""
  }
}