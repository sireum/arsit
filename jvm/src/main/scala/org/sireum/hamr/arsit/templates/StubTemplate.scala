// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir.FeatureCategory

object StubTemplate {
  // @formatter:off


  @pure def bridge(topLevelPackageName: String,
                   packageName: String,
                   imports: ISZ[ST],
                   bridgeName: String,
                   dispatchProtocol: Dispatch_Protocol.Type,
                   componentName: String,
                   componentType: String,
                   apiType: String,
                   ports: ISZ[Port],
                   dispatchTriggers: Option[ISZ[String]],
                   names: Names,
                   isBless: B): ST = {

    val _entryPoints = ISZ(EntryPoints.activate, EntryPoints.deactivate, EntryPoints.finalise, EntryPoints.initialise, EntryPoints.recover)

    val entryPoints: ISZ[ST] = _entryPoints.map((m: EntryPoints.Type) => {
      val isInitialize = m == EntryPoints.initialise
      val (apiId, apiType): (String, String) =
        if(isInitialize) (ApiTemplate.apiInitializationId, names.apiInitialization)
        else (ApiTemplate.apiOperationalId, names.apiOperational)

      var body = st"${componentName}.${m.string}(${apiId})"
      if (isInitialize) {
        body =
          st"""$body
              |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
      }

      st"""def ${m.string}(): Unit = {
          |  // implement the following method in '${componentName}':  def ${m.string}(api: ${apiType}): Unit = {}
          |  $body
          |}"""
    })

    val portParams: ISZ[String] = ports.map((p: Port) => {
      val artPortType: String = if (p.urgency.nonEmpty) "UrgentPort" else "Port"
      val portType = p.getPortTypeNames.qualifiedReferencedTypeName
      s"${p.name}: ${artPortType}[${portType}]"
    })

    val (apiIds, apiDecls): (ISZ[String], ISZ[ST]) = {
      val ids: ISZ[String] = ISZ(ApiTemplate.apiInitializationId, ApiTemplate.apiOperationalId)
      val decls: ISZ[ST] = ISZ(
        ApiTemplate.apiBridgeEntry(names, bridgeName, ports, T),
        ApiTemplate.apiBridgeEntry(names, bridgeName, ports, F))

      (ids, decls)
    }

    val stEntryPoints: ST = {
      val s = st"""val entryPoints : Bridge.EntryPoints =
                  |  ${bridgeName}.EntryPoints(
                  |    id,
                  |
                  |    ${(ports.map((p: Port) => s"${p.name}.id"), ",\n")},
                  |
                  |    dispatchTriggers,
                  |
                  |    ${(apiIds, ",\n")})"""
      s
    }

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import art._
          |import ${topLevelPackageName}._
          |${(imports.map((m: ST) => st"import $m"), "\n")}
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
          |  ${(apiDecls, "\n\n")}
          |
          |  ${stEntryPoints}
          |}
          |
          |object $bridgeName {
          |
          |  ${ApiTemplate.companionObjectApiInstances(names)}
          |
          |  @record class EntryPoints(
          |    ${bridgeName}Id : Art.BridgeId,
          |
          |    ${(ports.map((p: Port) => s"${addId(p.name)} : Art.PortId"), ",\n")},
          |
          |    dispatchTriggers : Option[ISZ[Art.PortId]],
          |
          |    ${(ApiTemplate.entryPointParams(names), ",\n")}) extends Bridge.EntryPoints {
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
          |      ${computeBody(s"${bridgeName}Id", componentName, names, ports, dispatchProtocol, F, isBless)}
          |    }
          |
          |    override
          |    def testCompute(): Unit = {
          |      ${computeBody(s"${bridgeName}Id", componentName, names, ports, dispatchProtocol, T, isBless)}
          |    }
          |
          |    ${(entryPoints, "\n\n")}
          |  }
          |}"""
    return ret
  }

  @pure def computeBody(bridgeName: String,
                        componentName: String,
                        names: Names,
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
            val ret = portCase(componentName, m, names.apiOperational, isFirst)
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
          val apiId = ApiTemplate.apiOperationalId
          val ret: ST =
            st"""Art.receiveInput(eventInPortIds, dataInPortIds)
                |
                |// implement the following in 'component':  def timeTriggered(api: ${names.apiOperational}): Unit = {}
                |${componentName}.timeTriggered(${apiId})
                |
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
          return ret
      }
    } else {
      val apiId = ApiTemplate.apiOperationalId
      val ret: ST =
        dispatchProtocol match {
        case Dispatch_Protocol.Sporadic =>
          st"""val EventTriggered(dispatchedPortIds) = Art.dispatchStatus(${bridgeName})
              |Art.receiveInput(dispatchedPortIds, dataInPortIds)
              |${componentName}.compute(${apiId}, dispatchedPortIds)
              |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
        case Dispatch_Protocol.Periodic =>
          st"""Art.receiveInput(eventInPortIds, dataInPortIds)
              |${componentName}.compute(${apiId}, ISZ())
              |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
      }

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
      st"""def get_${p.name}() : Option[${typeName}] = {
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
        st"""def put_${p.name}(${if (p.getPortTypeNames.isEmptyType()) "" else s"value : ${p.getPortTypeNames.qualifiedReferencedTypeName}"}) : Unit = {
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
        st"""def put_${p.name}(value : ${p.getPortTypeNames.qualifiedReferencedTypeName}) : Unit = {
            |  ${putValue(p)}
            |}"""
      return ret
    }
  }

  @pure def portApiUsage(p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      return st"""val apiUsage_${p.name}: Option[${typeName}] = api.get_${p.name}()
                 |api.logInfo(s"Received on ${p.name}: $${apiUsage_${p.name}}")"""
    } else {
      val payload: String =
        if (p.getPortTypeNames.isEmptyType()) ""
        else p.getPortTypeNames.example()

      return st"api.put_${p.name}($payload)"
    }
  }

  @pure def portCase(cname: String, v: Port, operationalApiType: String, first: B): ST = {
    val apiId = ApiTemplate.apiOperationalId
    val methodName = s"handle_${v.name}"
    val cMethodName = s"${cname}.${methodName}"

    v.feature.category match {
      case FeatureCategory.EventDataPort =>

        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}){
              |  val Some(${v.getPortTypeNames.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
              |
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}, value: ${v.getPortTypeNames.qualifiedReferencedTypeName}): Unit = {}
              |  ${cMethodName}(${apiId}, value)
              |}"""
        return ret
      case FeatureCategory.EventPort =>
        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}) {
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}): Unit = {}
              |  ${cMethodName}(${apiId})
              |}"""
        return ret
      case _ => halt(s"Unexpected ${v.feature.category}")
    }
  }

  @pure def componentImplBlock(componentType: String,
                               bridgeName: String,
                               names: Names,
                               dispatchProtocol: Dispatch_Protocol.Type,
                               ports: ISZ[Port],
                               isBless: B,
                               excludeComponentImpl: B): ST = {

    val inPorts = ports.filter(p => CommonUtil.isInPort(p.feature))
    val outPorts = ports.filter(p => CommonUtil.isOutPort(p.feature))

    def genMethod(signature: String, body: Option[ST]): ST = {
      val ret: ST = {
        if(body.nonEmpty) {
          st"""def ${signature}: Unit = {
              |  ${body}
              |}"""
        } else {
          st"def ${signature}: Unit = { }"
        }
      }
      return ret
    }

    val initSig: String = s"${EntryPoints.initialise.string}(api: ${names.apiInitialization})"
    val init: ST =
      if(excludeComponentImpl) { genMethod(initSig, None()) }
      else {
        val o: Option[ST] =
          if(outPorts.nonEmpty) Some(st"""
                                         |${(outPorts.map((p: Port) => portApiUsage(p)), "\n")}""")
          else None()
        genMethod(initSig, Some(st"""// example api usage
                                    |
                                    |api.logInfo("Example info logging")
                                    |api.logDebug("Example debug logging")
                                    |api.logError("Example error logging")
                                    |${o}"""))
      }

    val exampleApiGetterUsage: Option[ST] =
      if(excludeComponentImpl) { None() }
      else {
        Some(st"""// example api usage
                 |
                 |${(inPorts.map((p: Port) => portApiUsage(p)), "\n")}""")
      }
    val eventHandlers: ISZ[ST] =
      if (dispatchProtocol == Dispatch_Protocol.Periodic) {
        val ttsig: String = s"timeTriggered(api: ${names.apiOperational})"
        ISZ(if(excludeComponentImpl) genMethod(ttsig, None())
            else genMethod(ttsig, exampleApiGetterUsage))
      } else {
        val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
        var first = T
        inEventPorts.map(p => {
          val handlerName: String = s"handle_${p.name}"

          val (eventSig, receivedFeedback) : (String, String) = p.feature.category match {
            case FeatureCategory.EventDataPort =>
              val edpSig = s"$handlerName(api: ${names.apiOperational}, value : ${p.getPortTypeNames.qualifiedReferencedTypeName})"
              (edpSig, s"${"${value}"}")
            case FeatureCategory.EventPort =>
              val epSig = s"$handlerName(api: ${names.apiOperational})"
              (epSig, s"${p.name} event")
            case _ => halt(s"Unexpected ${p.feature.category}")
          }

          if(excludeComponentImpl) {
            genMethod(eventSig, None())
          } else {
            val stringInterp: String = if(CommonUtil.isAadlEventPort(p.feature)) "" else "s"
            var body = st"""api.logInfo("example $handlerName implementation")
                           |api.logInfo(${stringInterp}"received ${receivedFeedback}")"""
            if(first) { // only attach example api usage to first event handler
              first = F
              body = st"""$body
                         |${exampleApiGetterUsage}"""
            }
            genMethod(eventSig, Some(body))
          }
        })
      }

    val entryPoints = EntryPoints.elements.filter((f: EntryPoints.Type) => f != EntryPoints.compute && f != EntryPoints.initialise).map((m: EntryPoints.Type) => {
      st"""def ${m.string}(api: ${names.apiOperational}): Unit = { }"""
    })

    val ret: ST =
      st"""${StringTemplate.safeToEditComment()}
          |object $componentType {
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
                       returnType: String,
                       exampleValue: Option[ST]): (ST, ST) = {
    return (
      st"""def ${methodName}(${(params, ",\n")}): ${returnType} = ${"$"}""",
      st"""def ${methodName}(${(params, ",\n")}): ${returnType} = {
          |  ${if (returnType != "") st"return ${exampleValue.get}" else ""}
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

