// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir.FeatureCategory

object StubTemplate {
  // @formatter:off
  @pure def bridgeTestSuite(basePackage: String,
                            names: Names, ports: ISZ[Port]): ST = {

    val setters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isInFeature(p.feature)).map((p: Port) => {
      val (param, arg): (ST, ST) = p.feature.category match {
        case FeatureCategory.EventPort => (st"", st"Empty()")
        case _ => (st"value : ${p.getPortTypeNames.qualifiedReferencedTypeName}", st"${p.getPortTypeNames.qualifiedPayloadName}(value)")
      }

      st"""// setter for in ${p.feature.category}
          |def put_${p.name}(${param}): Unit = {
          |  ArtNative_Ext.insertInPortValue(bridge.api.${p.name}_Id, ${arg})
          |}
          |"""
    })

    val getters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isOutFeature(p.feature)).map((p: Port) => {
      val isEvent = p.feature.category == FeatureCategory.EventPort
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
      val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
      val value: String = if (isEvent) "Empty()" else "v"
      val payloadMethodName: String = s"get_${p.name}_payload()"

      st"""// getter for out ${p.feature.category}
          |def get_${p.name}(): Option[${typeName}] = {
          |  val value: Option[${typeName}] = ${payloadMethodName} match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) => fail(s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}
          |
          |// payload getter for out ${p.feature.category}
          |def ${payloadMethodName}: Option[${payloadType}] = {
          |  return ArtNative_Ext.observeOutPortValue(bridge.api.${addId(p.name)}).asInstanceOf[Option[${payloadType}]]
          |}
          |"""
    })

    val ret: ST =
      st"""package ${names.packageName}
          |
          |import art.{ArtNative_Ext, Empty}
          |import ${basePackage}._
          |import org.sireum._
          |
          |${StringTemplate.safeToEditComment()}
          |class ${names.testName} extends BridgeTestSuite[${names.bridge}](Arch.${names.instanceName}) {
          |  test("Example Unit Test"){
          |    executeTest()
          |  }
          |
          |  //////////////////////
          |  // HELPER FUNCTIONS //
          |  //////////////////////
          |
          |  ${(setters ++ getters, "\n")}
          |  def getComponent(): ${names.componentImpl} = {
          |    return bridge.entryPoints.asInstanceOf[${names.bridge}.EntryPoints].component
          |  }
          |}
          |"""
    return ret
  }

  @pure def bridge(topLevelPackageName: String,
                   packageName: String,
                   bridgeName: String,
                   dispatchProtocol: Dispatch_Protocol.Type,
                   componentName: String,
                   componentType: String,
                   componentImplType: String,
                   ports: ISZ[Port],
                   dispatchTriggers: Option[ISZ[String]],
                   types: AadlTypes,
                   isBless: B): ST = {

    val _entryPoints = ISZ(EntryPoints.activate, EntryPoints.deactivate, EntryPoints.finalise, EntryPoints.initialise, EntryPoints.recover)

    val entryPoints: ISZ[ST] = _entryPoints.map((m: EntryPoints.Type) => {
      var body = st"${componentName}.${m.string}()"
      if (m == EntryPoints.initialise) {
        body =
          st"""$body
              |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
      }

      st"""def ${m.string}: Unit = {
          |  $body
          |}"""
    })

    val portParams: ISZ[String] = ports.map((p: Port) => {
      val artPortType: String = if (p.urgency.nonEmpty) "UrgentPort" else "Port"
      val portType = p.getPortTypeNames.qualifiedReferencedTypeName
      s"${p.name}: ${artPortType}[${portType}]"
    })

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import ${topLevelPackageName}._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@record class $bridgeName(
          |  val id: Art.BridgeId,
          |  val name: String,
          |  val dispatchProtocol: DispatchPropertyProtocol,
          |  val dispatchTriggers: Option[ISZ[Art.PortId]],
          |
          |  ${(portParams, ",\n")}
          |  ) extends Bridge {
          |
          |  val ports : Bridge.Ports = Bridge.Ports(
          |    all = ISZ(${(ports.map((m: Port) => m.name), ",\n")}),
          |
          |    dataIns = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    dataOuts = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventIns = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventOuts = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")})
          |  )
          |
          |  val api : ${bridgeName}.Api =
          |    ${bridgeName}.Api(
          |      id,
          |      ${(ports.map((p: Port) => s"${p.name}.id"), ",\n")}
          |    )
          |
          |  val entryPoints : Bridge.EntryPoints =
          |    ${bridgeName}.EntryPoints(
          |      id,
          |
          |      ${(ports.map((p: Port) => s"${p.name}.id"), ",\n")},
          |
          |      dispatchTriggers,
          |
          |      ${componentImplType}(api)
          |    )
          |}
          |
          |object $bridgeName {
          |
          |  @record class Api(
          |    id : Art.BridgeId,
          |    ${(ports.map((p: Port) => s"${addId(p.name)} : Art.PortId"), ",\n")}) {
          |
          |    ${(ports.filter((p: Port) => CommonUtil.isEventPort(p.feature)).map((p: Port) => eventPortApi(p).render), "\n\n")}
          |
          |    ${(ports.filter((p: Port) => CommonUtil.isAadlDataPort(p.feature)).map((p: Port) => dataPortApi(p).render), "\n\n")}
          |
          |    def logInfo(msg: String): Unit = {
          |      Art.logInfo(id, msg)
          |    }
          |
          |    def logDebug(msg: String): Unit = {
          |      Art.logDebug(id, msg)
          |    }
          |
          |    def logError(msg: String): Unit = {
          |      Art.logError(id, msg)
          |    }
          |  }
          |
          |  @record class EntryPoints(
          |    ${bridgeName}Id : Art.BridgeId,
          |
          |    ${(ports.map((p: Port) => s"${addId(p.name)} : Art.PortId"), ",\n")},
          |
          |    dispatchTriggers : Option[ISZ[Art.PortId]],
          |
          |    $componentName : $componentImplType ) extends Bridge.EntryPoints {
          |
          |    val dataInPortIds: ISZ[Art.PortId] = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |    val eventInPortIds: ISZ[Art.PortId] = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |    val dataOutPortIds: ISZ[Art.PortId] = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |    val eventOutPortIds: ISZ[Art.PortId] = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((p: Port) => addId(p.name)), ",\n")})
          |
          |    def compute(): Unit = {
          |      ${computeBody(s"${bridgeName}Id", componentName, ports, dispatchProtocol, F, isBless)}
          |    }
          |
          |    override
          |    def testCompute(): Unit = {
          |      ${computeBody(s"${bridgeName}Id", componentName, ports, dispatchProtocol, T, isBless)}
          |    }
          |
          |    ${(entryPoints, "\n\n")}
          |  }
          |}"""
    return ret
  }

  @pure def computeBody(bridgeName: String,
                        componentName: String,
                        ports: ISZ[Port],
                        dispatchProtocol: Dispatch_Protocol.Type,
                        isTesting: B,
                        isBless: B
                       ): ST = {
    val sendOutputName: String = if (isTesting) "releaseOutput" else "sendOutput"

    if (!isBless) {
      dispatchProtocol match {
        case Dispatch_Protocol.Sporadic =>
          var isFirst = T
          val cases: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isEventPort(p.feature) && CommonUtil.isInFeature(p.feature)).map((m: Port) => {
            val ret = portCase(componentName, m, isFirst)
            isFirst = F
            ret
          })
          val ret: ST =
            st"""// transpiler friendly filter
                |def filter(receivedEvents: ISZ[Art.PortId], triggers: ISZ[Art.PortId]): ISZ[Art.PortId] = {
                |  var r = ISZ[Art.PortId]()
                |  val opsTriggers = ops.ISZOps(triggers)
                |  for(e <- receivedEvents) {
                |    if(opsTriggers.contains(e)) {
                |      r = r :+ e
                |    }
                |  }
                |  return r
                |}
                |
                |// fetch received events ordered by highest urgency then earliest arrival-time
                |val EventTriggered(receivedEvents) = Art.dispatchStatus(${bridgeName})
                |
                |// remove non-dispatching event ports
                |val dispatchableEventPorts: ISZ[Art.PortId] =
                |  if(dispatchTriggers.isEmpty) receivedEvents
                |  else filter(receivedEvents, dispatchTriggers.get)
                |
                |Art.receiveInput(eventInPortIds, dataInPortIds)
                |
                |for(portId <- dispatchableEventPorts) {
                |  ${(cases, "\n")}
                |}
                |
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
          return ret
        case Dispatch_Protocol.Periodic =>
          val ret: ST =
            st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                |${componentName}.timeTriggered()
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
          return ret
      }
    } else {
      val ret: ST =
        st"""Art.receiveInput(eventInPortIds, dataInPortIds)
            |${componentName}.Compute_Entrypoint()
            |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
      return ret
    }
  }

  @pure def demo(): ST = {
    val ret: ST =
      st"""object Demo extends App {
          |  art.Art.run(Arch.ad)
          |}"""
    return ret
  }

  @pure def componentTrait(topLevelPackageName: String,
                           packageName: String,
                           dispatchProtocol: Dispatch_Protocol.Type,
                           componentType: String,
                           bridgeName: String,
                           ports: ISZ[Port]): ST = {
    val entryPoints = EntryPoints.elements.filter((f: EntryPoints.Type) => f != EntryPoints.compute).map((m: EntryPoints.Type) => st"def ${m.string}(): Unit = {}")

    val caseMethods: ISZ[ST] = dispatchProtocol match {
      case Dispatch_Protocol.Sporadic =>
        ports.filter(p => CommonUtil.isEventPort(p.feature) && CommonUtil.isInFeature(p.feature)).map(m => portCaseMethod(m, F))
      case Dispatch_Protocol.Periodic => ISZ(st"""def timeTriggered() : Unit = {}""")
    }

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@msig trait ${componentType} {
          |
          |  def api : ${bridgeName}.Api
          |
          |  ${(caseMethods, "\n\n")}
          |
          |  ${(entryPoints, "\n\n")}
          |}"""
    return ret
  }

  @pure def addId(s: String): String = {
    return s"${s}_Id"
  }

  @pure def putValue(p: Port): ST = {
    return st"""Art.putValue(${addId(p.name)}, ${p.getPortTypeNames.qualifiedPayloadName}${if (p.getPortTypeNames.isEmptyType()) "()" else "(value)"})"""
  }

  @pure def apiCall(componentName: String, portName: String): String = {
    return s"${componentName}.${portName}Api(${portName}.id)"
  }

  @pure def apiSig(portName: String, portType: String): ST = {
    return st"""${portName} : ${portName}Api"""
  }

  @pure def getterApi(p: Port): ST = {
    val isEvent = CommonUtil.isAadlEventPort(p.feature)
    val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
    val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
    val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
    val value: String = if (isEvent) "Empty()" else "v"

    val ret: ST =
      st"""def get${p.name}() : Option[${typeName}] = {
          |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) =>
          |      Art.logError(id, s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |      None[${typeName}]()
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}"""
    return ret
  }

  @pure def eventPortApi(p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      return getterApi(p)
    } else {
      val ret: ST =
        st"""def send${p.name}(${if (p.getPortTypeNames.isEmptyType()) "" else s"value : ${p.getPortTypeNames.qualifiedReferencedTypeName}"}) : Unit = {
            |  ${putValue(p)}
            |}"""
      return ret
    }
  }

  @pure def dataPortApi(p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      return getterApi(p)
    } else {
      val ret: ST =
        st"""def set${p.name}(value : ${p.getPortTypeNames.qualifiedReferencedTypeName}) : Unit = {
            |  ${putValue(p)}
            |}"""
      return ret
    }
  }

  @pure def portApiUsage(p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      return st"val apiUsage_${p.name}: Option[${typeName}] = api.get${p.name}()"
    } else {
      val payload: String =
        if (p.getPortTypeNames.isEmptyType()) ""
        else p.getPortTypeNames.empty()

      val methodName: String =
        if (CommonUtil.isAadlDataPort(p.feature)) "set"
        else "send"

      return st"api.${methodName}${p.name}($payload)"
    }
  }

  @pure def portCase(cname: String, v: Port, first: B): ST = {
    v.feature.category match {
      case FeatureCategory.EventDataPort =>
        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}){
              |  val Some(${v.getPortTypeNames.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
              |  ${cname}.handle${v.name}(value)
              |}"""
        return ret
      case FeatureCategory.EventPort =>
        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}) {
              |  ${cname}.handle${v.name}()
              |}"""
        return ret
      case _ => halt(s"Unexpected ${v.feature.category}")
    }
  }

  @pure def portCaseMethod(v: Port, isImpl: B): ST = {
    val or: String = if (isImpl) "override " else ""
    val ed: String = if (isImpl) "example" else "default"
    val methodName: String = s"handle${v.name}"

    v.feature.category match {
      case FeatureCategory.EventDataPort =>
        val ret: ST =
          st"""${or}def $methodName(value : ${v.getPortTypeNames.qualifiedReferencedTypeName}): Unit = {
              |  api.logInfo("${ed} $methodName implementation")
              |  api.logInfo(s"received ${"${value}"}")
              |}"""
        return ret
      case FeatureCategory.EventPort =>
        val ret: ST =
          st"""${or}def $methodName(): Unit = {
              |  api.logInfo("${ed} $methodName implementation")
              |  api.logInfo("received ${v.name}")
              |}"""
        return ret
      case _ => halt(s"Unexpected ${v.feature.category}")
    }
  }

  @pure def componentImplBlock(componentType: String,
                               bridgeName: String,
                               componentImplType: String,
                               dispatchProtocol: Dispatch_Protocol.Type,
                               ports: ISZ[Port],
                               isBless: B): ST = {

    val init =
      st"""override def ${EntryPoints.initialise.string}(): Unit = {
          |  // example api usage
          |
          |  api.logInfo("Example info logging")
          |  api.logDebug("Example debug logging")
          |  api.logError("Example error logging")
          |
          |  ${(ports.map((p: Port) => portApiUsage(p)), "\n")}
          |}"""

    val entryPoints = EntryPoints.elements.filter((f: EntryPoints.Type) => f != EntryPoints.compute && f != EntryPoints.initialise).map((m: EntryPoints.Type) => {
      st"""override def ${m.string}(): Unit = {
          |  // example override of ${m.string}
          |}"""
    })

    val eventHandlers: ISZ[ST] =
      if (dispatchProtocol == Dispatch_Protocol.Periodic) {
        ISZ(
          st"""override def timeTriggered(): Unit = {
              |  // example override of timeTriggered
              |}""")
      } else {
        ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature)).map(m => portCaseMethod(m, T))
      }

    val ret: ST =
      st"""${StringTemplate.safeToEditComment()}
          |@record class $componentImplType (val api : ${bridgeName}.Api) ${if (isBless) "" else s"extends ${componentType}"} {
          |
          |  ${init}
          |
          |  ${(eventHandlers, "\n\n")}
          |
          |  ${(entryPoints, "\n\n")}
          |}"""
    return ret
  }

  @pure def subprogram(methodName: String,
                       params: ISZ[String],
                       returnType: String): (ST, ST) = {
    return (
      st"""def ${methodName}(${(params, ",\n")}): ${returnType} = ${"$"}""",
      st"""def ${methodName}(${(params, ",\n")}): ${returnType} = {
          |  ${if (returnType != "") s"return ${returnType}()" else ""}
          |}""")
  }

  @pure def slangPreamble(inSlang: B,
                          packageName: String,
                          topLevelPackageName: String,
                          blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""${if (inSlang) "// #Sireum\n\n" else ""}package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |
          |${(blocks, "\n\n")}
          |"""
    return ret
  }

  @pure def slangBody(slangAnnotation: String,
                      objectName: String,
                      body: ISZ[ST]): ST = {
    val ret: ST =
      st"""${slangAnnotation}object ${objectName} {
          |
          |  ${(body, "\n\n")}
          |}"""
    return ret
  }

  // @formatter:on
}

