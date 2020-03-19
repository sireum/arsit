// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._

object Sel4NixTemplate {
  val isolatedArchitectureName: String = "IsolatedArch"
  
  def sendOutput(entries: ST): ST = {
    val ret: ST = st"""def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                      |  // ignore params
                      |    
                      |  ${entries}
                      |}"""
    return ret
  }

  def getValue(entries: ST): ST = {
    val ret: ST = st"""def getValue(portId: Art.PortId): Option[DataContent] = {
                      |  ${entries}
                      |}"""
    return ret
    
  }

  def putValue(entries: ST): ST = {
    val ret: ST = st"""def putValue(portId: Art.PortId, data: DataContent): Unit = {
                      |  ${entries}
                      |}"""
    return ret
  }

  def receiveInput(entries: ST): ST = {
    val ret: ST = st"""def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                      |  // ignore params
                      |  
                      |  ${entries}  
                      |}"""
    return ret
  }

  def portVariable(instanceName: String,
                   portName: String,
                   portId: String): ST = {
    val ret: ST = st"""val ${portId}: Art.PortId = ${isolatedArchitectureName}.${instanceName}.${portName}.id
                      |var ${portName}: Option[DataContent] = noData"""
    return ret
  }
  
  def extensionObjectStub(packageName: String,
                          instanceName: String,
                          name: String,
                          entries: ST): ST = {
    val ret: ST = st"""package ${packageName}.${instanceName}
                      |
                      |import org.sireum._
                      |import art._
                      |import ${packageName}._
                      |
                      |object ${name} {
                      |  ${entries}
                      |}
                      |"""
    return ret
  }
  
  def extensionObject(name: String,
                      entries: ST): ST = {
    val ret: ST = st"""@ext object ${name} {
                      |  ${entries}
                      |}"""
    return ret
  }
  
  def dispatchStatus(body: ST): ST = {
    val ret: ST = st"""def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
                      |  ${body}
                      |}"""
    return ret
  }
  
  def main(packageName: String,
           instanceName: String,
           dispatchStatus: ST,
           globals: ST,
           receiveInput: ST,
           getValue: ST,
           putValue: ST,
           sendOutput: ST,
           extensionObject: ST,
           typeTouches: ST): ST = {
    val ret: ST = st"""// #Sireum
                   |
                   |package ${packageName}.${instanceName}
                   |
                   |import org.sireum._
                   |import art._
                   |import ${packageName}._
                   |
                   |object ${instanceName} extends App {
                   |
                   |  val entryPoints: Bridge.EntryPoints = ${isolatedArchitectureName}.${instanceName}.entryPoints
                   |  val noData: Option[DataContent] = None()
                   |  
                   |  ${globals}
                   |
                   |  ${dispatchStatus}
                   |
                   |  def initialiseArchitecture(): Unit = {
                   |    Art.run(${isolatedArchitectureName}.ad)
                   |  }
                   |                     
                   |  ${getValue}
                   |  
                   |  ${receiveInput}
                   |
                   |  ${putValue}
                   |  
                   |  ${sendOutput}
                   |  
                   |  def main(args: ISZ[String]): Z = {
                   |
                   |    // need to touch the following for transpiler
                   |    initialiseArchitecture()
                   |    entryPoints.initialise()
                   |    entryPoints.compute()
                   |
                   |    // call empty on every type in case some are only used as a field in a record
                   |    ${typeTouches}
                   |    
                   |    return 0
                   |  }
                   |
                   |  def logInfo(title: String, msg: String): Unit = {
                   |    print(title)
                   |    print(": ")
                   |    println(msg)
                   |  }
                   |
                   |  def logError(title: String, msg: String): Unit = {
                   |    eprint(title)
                   |    eprint(": ")
                   |    eprintln(msg)
                   |  }
                   |
                   |  def logDebug(title: String, msg: String): Unit = {
                   |    print(title)
                   |    print(": ")
                   |    println(msg)
                   |  }
                   |  
                   |  def run(): Unit = {}
                   |
                   |}
                   |
                   |${extensionObject}
                   |"""
    return ret
  }



  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if(options.size > 0) { Some(options(0)) } else { None() }
    val rest: ISZ[(ST, ST)] = if(options.size > 1) { org.sireum.ops.ISZOps(options).drop(1) } else { ISZ() }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {

    var body = st""

    if(ifbranch.nonEmpty) {
      body = st"""if(${ifbranch.get._1}) {
                 |  ${ifbranch.get._2}
                 |} """
    }

    if(elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) => st"""else if(${x._1}) {
                                               |  ${x._2}
                                               |} """)
      body = st"""${body}${ei}"""
    }

    if(els.nonEmpty) {
      if(ifbranch.nonEmpty) {
        body = st"""${body}else {
                   |  ${els.get}
                   |}"""
      } else {
        body = els.get
      }
    }

    return body
  }
}
