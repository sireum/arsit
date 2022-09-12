// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.gcl.GumboGen.{GclEntryPointInitialize, GclEntryPointPeriodicCompute, GclEntryPointSporadicCompute}
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.symbols.{AadlPort, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.{CommonUtil, NameProvider}
import org.sireum.hamr.ir.FeatureCategory

object StubTemplate {
  // @formatter:off


  def addImports(imports: ISZ[ST]): Option[ST] = {
    val s: Set[String] = Set.empty[String] ++ (imports.map((m: ST) => s"import ${m.render}"))
    return if(s.nonEmpty) Some(st"${(s.elements, "\n")}") else None()
  }

  // generate code for the (fully quantified component name)_bridge.scala file within the
  // project's src/main/bridge folder
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
                   names: NameProvider,
                   isBless: B): ST = {

    // make sequence of tags for each category of Entry Point (EP) for which code will be generated
    // Note: This list does not include "compute" -- that's handled separately as a special case because its
    // structure varies based on the thread dispatch protocol and whether or not we are generated code for BLESS
    val _entryPoints = ISZ(EntryPoints.activate, EntryPoints.deactivate, EntryPoints.finalise, EntryPoints.initialise, EntryPoints.recover)

    // iterate through (map) each EP category tag and build a Slang String Template (ST)
    // for a method for each EP
    val entryPoints: ISZ[ST] = _entryPoints.map((m: EntryPoints.Type) => {
    // ------- for each EP category ...
      // generate flag value indicating if we are processing an Initialize EP
      val isInitialize = m == EntryPoints.initialise
      // if this is an Initialize EP, then set the generated method
      // to take as an argument the (restricted) Initialize API (which doesn't have port getters).
      // Otherwise, use the more general Operational API (which has both port getters and setters).
      val (apiId, apiType): (String, String) =
        if(isInitialize) (ApiTemplate.apiInitializationId, names.apiInitialization)
        else (ApiTemplate.apiOperationalId, names.apiOperational)
      // create string representing the call to the application entry point method.
      // This call will be the primary code in the bridge entry point method.
      //   e.g.,  component.finalise(operational_api)
      var body = st"${componentName}.${m.string}(${apiId})"
      // if the EP is an initialize EP,
      // append the sendOutput RTS call after the call to the application initial EP code.
      // This propagates (to the communication substrate) the application code's writes to
      // out ports.
      if (isInitialize) {
        body =
          st"""$body
              |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""
      }

      // form the complete method def for the EP
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
          |${addImports(imports)}
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@datatype class $bridgeName(
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
          |  @datatype class EntryPoints(
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
          |    override
          |    def testInitialise(): Unit = {
          |      ${initialiseBody(s"${bridgeName}Id", componentName, names, T)}
          |    }
          |
          |    ${(entryPoints, "\n\n")}
          |  }
          |}"""
    return ret
  }

  @pure def computeBody(bridgeName: String,
                        componentName: String,
                        names: NameProvider,
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

  @pure def initialiseBody(bridgeName: String,
                        componentName: String,
                        names: Names,
                        isTesting: B
                       ): ST = {
    // determine communication substrate method name based on if this is test infrastructure
    //  or a "regular" entry point call
    val sendOutputName: String = if (isTesting) "releaseOutput" else "sendOutput"

    // generate call to application code Initialize EP
    //  e.g., component.initialise(initialization_api)
    // generate call to propagate writes to output ports to the communication substrate
    val ret: ST =
           st"""${componentName}.${EntryPoints.initialise.string}(${ApiTemplate.apiInitializationId})
                |Art.${sendOutputName}(eventOutPortIds, dataOutPortIds)"""
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
                               names: NameProvider,
                               dispatchProtocol: Dispatch_Protocol.Type,
                               ports: ISZ[Port],
                               isBless: B,
                               excludeComponentImpl: B,
                               preBlocks: ISZ[ST],
                               entryPointContracts: Map[EntryPoints.Type, GumboGen.GclEntryPointContainer],
                               symbolTable: SymbolTable): ST = {

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
    val contract: Option[ST] = entryPointContracts.get(EntryPoints.initialise) match {
      case Some(x: GclEntryPointInitialize) => Some(x.contract)
      case Some(x) => halt(s"Infeasible ${x}")
      case _ => None()
    }

    val init: ST =
      if(excludeComponentImpl) {
        genMethod(initSig, contract)
      }
      else {
        val o: Option[ST] =
          if(outPorts.nonEmpty) Some(st"""
                                         |${(outPorts.map((p: Port) => portApiUsage(p)), "\n")}""")
          else None()
        genMethod(initSig, Some(st"""${contract}
                                    |// example api usage
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

    def comma(oo: Option[ST]): Option[ST] = { return if (oo.isEmpty) None() else Some(st"${oo.get},")}
    val eventHandlers: ISZ[ST] =
      if (dispatchProtocol == Dispatch_Protocol.Periodic) {
        val optContract: Option[ST] = entryPointContracts.get(EntryPoints.compute) match {
          case Some(g: GclEntryPointPeriodicCompute) =>
            Some(st"""Contract(
                     |  ${comma(g.requires)}
                     |  ${comma(g.modifies)}
                     |  ${g.ensures}
                     |)""")
          case _ => None()
        }

        val ttsig: String = s"timeTriggered(api: ${names.apiOperational})"
        ISZ(if(excludeComponentImpl) genMethod(ttsig, optContract)
            else genMethod(ttsig, Some(st"""$optContract
                                           |$exampleApiGetterUsage""")))
      } else {
        val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
        var first = T
        inEventPorts.map(p => {
          val aadlPort = symbolTable.featureMap.get(p.feature.identifier.name).get.asInstanceOf[AadlPort]

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

          val optContract: Option[ST] = entryPointContracts.get(EntryPoints.compute) match {
            case Some(g: GclEntryPointSporadicCompute) =>
              val handler = g.handlers.get(aadlPort).get

              Some(st"""Contract(
                       |  ${comma(handler.requires)}
                       |  ${comma(handler.modifies)}
                       |  ${handler.ensures}
                       |)""")
            case _ => None()
          }

          if(excludeComponentImpl) {
            genMethod(eventSig, optContract)
          } else {
            val stringInterp: String = if(CommonUtil.isAadlEventPort(p.feature)) "" else "s"
            var body = st"""${optContract}
                           |api.logInfo("example $handlerName implementation")
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

    val preBlocksOpt: Option[ST] = if(preBlocks.isEmpty) None()
    else Some(st"""${(preBlocks, "\n")}
                  |""")

    val ret: ST =
      st"""${StringTemplate.safeToEditComment()}
          |object $componentType {
          |
          |  ${preBlocksOpt}
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
                       returnType: Option[String],
                       exampleValue: Option[ST]): (ST, ST) = {
    val _returnType: String = if(returnType.nonEmpty) returnType.get else "Unit"
    return (
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = ${"$"}""",
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = {
          |  ${if (exampleValue.nonEmpty) st"return ${exampleValue.get}" else ""}
          |}""")
  }

  @pure def slangPreamble(inSlang: B,
                          packageName: String,
                          topLevelPackageName: String,
                          imports: ISZ[ST],
                          blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""${if (inSlang) "// #Sireum\n\n" else ""}package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |${addImports(imports)}
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

