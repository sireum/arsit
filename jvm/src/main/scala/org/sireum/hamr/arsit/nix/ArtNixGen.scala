package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.symbols.{Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlTypes, DataTypeNames, TypeUtil}

case class ArtNixGen(val dirs: ProjectDirectories,
                     val cExtensionDir: String,
                     val model: Aadl,
                     val arsitOptions: Cli.ArsitOption,
                     val symbolTable: SymbolTable,
                     val types: AadlTypes,
                     val previousPhase: Result) extends NixGen {

  var cOutputDir : String = _

  val basePackage: String = arsitOptions.packageName

  var resources: ISZ[Resource] = ISZ()

  var transpilerOptions: ISZ[CTranspilerOption] = ISZ()

  var maxPortsForComponents: Z = 0
  var numConnections: Z = 0
  var maxStackSize: Z = -1
  
  var portId: Z = previousPhase.maxPort
  def getPortId(): Z = {
    val r = portId
    portId = portId + 1
    return r
  }

  def generate(): ArsitResult = {
    assert(SlangUtil.isNix(arsitOptions.platform))

    cOutputDir = if (arsitOptions.outputCDir.nonEmpty) {
      arsitOptions.outputCDir.get
    } else {
      SlangUtil.pathAppend(dirs.srcDir, ISZ("c", "nix"))
    }

    gen(model)

    return ArsitResult(
      previousPhase.resources() ++ resources,
      portId,
      previousPhase.maxComponent, 
      transpilerOptions)
  }

  def addExeResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ SlangUtil.createExeResource(outDir, path, content, overwrite)
  }

  def addResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ SlangUtil.createResource(outDir, path, content, overwrite)
  }

  def gen(model: Aadl): Unit = {
    assert(model.components.size == 1)
    assert(CommonUtil.isSystem(model.components(0)))

    var platformPorts: ISZ[ST] = ISZ()
    var platformPayloads: ISZ[ST] = ISZ()
    var mainSends: ISZ[ST] = ISZ()
    var inPorts: ISZ[Port] = ISZ()
    var aepNames: ISZ[String] = ISZ()
    var appNames: ISZ[String] = ISZ()

    val components = symbolTable.airComponentMap.entries.filter(p =>
      CommonUtil.isThread(p._2) || (CommonUtil.isDevice(p._2) && arsitOptions.devicesAsThreads))

    var transpilerExtensions: ISZ[Os.Path] = getExistingCFiles(cExtensionDir)
    
    for ((archVarName, component) <- components) {

      val names: Names = Names(component, basePackage)
      val ports: ISZ[Port] = Util.getPorts(component, types, basePackage, z"0")

      val dispatchProtocol = PropertyUtil.getDispatchProtocol(component) match {
        case Some(x) => x
        case _ =>
          if(CommonUtil.isDevice(component)) {
            Dispatch_Protocol.Periodic
          } else {
            halt("HAMR codegen only supports Periodic or Sporadic threads")
          }
      }

      val isPeriodic: B = dispatchProtocol == Dispatch_Protocol.Periodic

      val bridgeInstanceVarName: String = CommonUtil.getName(component.identifier)
      val AEP_Id: String = s"${names.instanceName}_AEP"
      val App_Id: String = s"${names.instanceName}_App"

      val AEPPayloadTypeName: String = s"${AEP_Id}_Payload"

      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portIdOpts: ISZ[ST] = ISZ()

      var portOptResets: ISZ[ST] = ISZ()
      var aepPortCases: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      PropertyUtil.getStackSizeInBytes(component) match {
        case Some(bytes) => if(bytes > maxStackSize) { 
          maxStackSize = bytes
        }
        case _ =>
      }
      
      val featureEnds = Util.getFeatureEnds(component.features)
      val portSize = featureEnds.filter(f => CommonUtil.isInFeature(f) || CommonUtil.isOutFeature(f)).size
      if(portSize > maxPortsForComponents) {
        maxPortsForComponents = portSize
      }

      val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)
      
      for (p <- featureEnds if CommonUtil.isInFeature(p)) {
        assert (CommonUtil.isPort(p))

        val portName = CommonUtil.getLastName(p.identifier)
        val isTrigger = if(dispatchTriggers.isEmpty) T else {
          dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
        }
        val port = Util.getPort(p, component, types, basePackage, isTrigger, z"-1000")

        val portIdName: String = s"${port.name}PortId"
        val portOptName: String = s"${port.name}Opt"
        val portType: String = port.getPortTypeNames.qualifiedReferencedTypeName
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${port.name}"

        portDefs = portDefs :+ Template.portDef(portOptName, portType)
        portOpts = portOpts :+ Template.portOpt("", portOptName, portType, T)

        portIds :+= Template.portId(portIdName, archPortInstanceName)
        portIdOpts :+= Template.portOpt(portIdName, portOptName, "Art.PortId", F)

        aepPortCases = aepPortCases :+ Template.aepPortCase(portIdName, portOptName, port.getPortTypeNames, CommonUtil.isAadlDataPort(p))

        portOptResets = portOptResets :+ Template.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ Template.appCases(portOptName, portIdName, port.getPortTypeNames)

        inPorts :+= port
      }

      platformPorts = platformPorts :+ Template.platformPortDecl(App_Id, getPortId())
      mainSends :+= Template.mainSend(App_Id)
      appNames = appNames :+ App_Id

      val AEP_Payload = Template.AEPPayload(AEPPayloadTypeName, portOptNames)
      if(portOpts.nonEmpty && arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) {
        platformPorts :+= Template.platformPortDecl(AEP_Id, getPortId())
        platformPayloads :+= Template.platformPayload(AEPPayloadTypeName, portDefs)

        mainSends :+= Template.mainSend(AEP_Id)
        aepNames = aepNames :+ AEP_Id

        val stAep = Template.aep(basePackage, AEP_Id, portOpts,
          portIds, portOptResets, aepPortCases, AEP_Id, App_Id, AEP_Payload, isPeriodic)

        addResource(dirs.nixDir, ISZ(basePackage, s"${AEP_Id}.scala"), stAep, T)
      }

      val stApp = Template.app(basePackage, App_Id, App_Id,
        AEP_Id, Util.getPeriod(component), bridgeInstanceVarName, AEP_Payload, portIds, appCases, component, isPeriodic)

      addResource(dirs.nixDir, ISZ(basePackage, s"${App_Id}.scala"), stApp, T)
      
      val (paths, extResources) = genExtensionFiles(component, names, ports)
      resources = resources ++ extResources
      transpilerExtensions = transpilerExtensions ++ paths
    }

    var artNixCasesM: HashSMap[String, ISZ[ST]] = HashSMap.empty
    for(c <- symbolTable.connections) {
      val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get
      val srcComp = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get

      if((CommonUtil.isDevice(srcComp) || CommonUtil.isThread(srcComp)) &&
         (CommonUtil.isDevice(dstComp) || CommonUtil.isThread(dstComp))) {
        val dstPath = CommonUtil.getName(c.dst.feature.get)
        val dstArchPortInstanceName = s"${CommonUtil.getName(dstComp.identifier)}.${CommonUtil.getLastName(c.dst.feature.get)}"
        val name: Names = Names(dstComp, basePackage)

        val srcArchPortInstanceName =
          s"${CommonUtil.getName(srcComp.identifier)}.${CommonUtil.getLastName(c.src.feature.get)}"

        if (inPorts.map(_.path).elements.contains(dstPath) &&
          (CommonUtil.isThread(srcComp) || CommonUtil.isDevice(srcComp)) && (CommonUtil.isThread(dstComp) || CommonUtil.isDevice(dstComp))) {
          val dstComp = if (arsitOptions.ipc == Cli.IpcMechanism.SharedMemory) s"${name.instanceName}_App" else s"${name.instanceName}_AEP"
          var cases: ISZ[ST] = {
            if (artNixCasesM.contains(srcArchPortInstanceName)) {
              artNixCasesM.get(srcArchPortInstanceName).get
            } else {
              ISZ()
            }
          }
          cases = cases :+ Template.artNixCase(dstComp, dstArchPortInstanceName)
          artNixCasesM = artNixCasesM + (srcArchPortInstanceName, cases)
        }
        numConnections = numConnections + 1
      } else {
        reporter.info(None(), Util.toolName, s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${CommonUtil.getName(c.name)}")
      }
    }

    platformPorts = platformPorts :+ Template.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case Cli.IpcMechanism.MessageQueue =>

        addResource(dirs.nixDir, ISZ(basePackage, "MessageQueue.scala"), Template.MessageQueue(basePackage), T)
        addResource(dirs.nixDir, ISZ(basePackage, "MessageQueue_Ext.scala"), Template.MessageQueueExt(basePackage), T)

      case Cli.IpcMechanism.SharedMemory =>

        addResource(dirs.nixDir, ISZ(basePackage, "SharedMemory.scala"), Template.SharedMemory(basePackage), T)
        addResource(dirs.nixDir, ISZ(basePackage, "SharedMemory_Ext.scala"), Template.SharedMemory_Ext(basePackage), T)
    }

    val stIPC = Template.ipc(basePackage, platformPorts, platformPayloads)
    addResource(dirs.nixDir, ISZ(basePackage, "IPC.scala"), stIPC, T)

    val artNixCases: ISZ[ST] = artNixCasesM.entries.map(k => Template.artNixCases(k._1, k._2))
    val stArtNix = Template.artNix(basePackage, artNixCases,
      inPorts.filter(p => CommonUtil.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id"))

    addResource(dirs.nixDir, ISZ(basePackage, "ArtNix.scala"), stArtNix, T)

    val stMain = Template.main(basePackage, mainSends)
    addResource(dirs.nixDir, ISZ(basePackage, "Main.scala"), stMain, T)

    addResource(dirs.nixDir, ISZ(basePackage, "Platform.scala"), Template.platform(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Platform_Ext.scala"), Template.PlatformExt(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "PlatformNix.scala"), Template.PlatformNix(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Process.scala"), Template.Process(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Process_Ext.scala"), Template.ProcessExt(basePackage), T)

    val platName = ops.StringOps(arsitOptions.platform.name).firstToLower

    addExeResource(dirs.binDir, ISZ(s"compile-${platName}.sh"), Template.compile(arsitOptions.platform), T)
    addExeResource(dirs.binDir, ISZ(s"run-${platName}.sh"), Template.run(aepNames, appNames, arsitOptions.platform), T)
    addExeResource(dirs.binDir, ISZ("stop.sh"), Template.stop(
      (if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aepNames else ISZ[String]()) ++ appNames), T)

    addResource(cExtensionDir, ISZ("ipc.c"), Util.getIpc(arsitOptions.ipc, basePackage), T)

    var outputPaths: ISZ[String] = ISZ(dirs.srcMainDir)
    if(!org.sireum.ops.StringOps(dirs.nixDir).contains(dirs.srcMainDir))
      outputPaths = outputPaths :+ dirs.nixDir

    val excludes:ISZ[String] = if(arsitOptions.excludeImpl) {
      for ((archVarName, m) <- components) yield {
        val name: Names = Names(m, basePackage)
        s"${name.packageName}.${name.componentImpl}"
      }
    } else {
      ISZ()
    }
    
    val transpileScriptName = "transpile.sh"
    val buildApps: B = T
    val additionalInstructions: Option[ST] = None()

    transpilerExtensions = transpilerExtensions :+ Os.path(cExtensionDir) / "ipc.c"

    val arraySizeInfluencers = ISZ(arsitOptions.maxArraySize, portId, previousPhase.maxComponent)

    val maxArraySize: Z = CommonUtil.findMaxZ(arraySizeInfluencers)

    val numPorts: Z = portId
    val numComponents: Z = previousPhase.maxComponent
    
    var customSequenceSizes: ISZ[String] = ISZ(
      s"MS[Z,art.Bridge]=${numComponents}",
      s"MS[Z,MOption[art.Bridge]]=${numComponents}",      
      s"IS[Z,art.UPort]=${maxPortsForComponents}",
      s"IS[Z,art.UConnection]=${numConnections}"
      
      // not valid
      //s"MS[org.sireum.Z,org.sireum.Option[art.UPort]]=${maxPortsForComponents}"
    )

    if(types.rawConnections) {
      TypeUtil.getMaxBitsSize(types) match {
        case Some(z) =>
          customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${z}"
        case _ => halt("Raw connections specified but couldn't determine max bit size")
      }
    }

    val customConstants: ISZ[String] = ISZ(
      s"art.Art.maxComponents=${numComponents}",
      s"art.Art.maxPorts=${numPorts}"
    )
    
    val _extensions: ISZ[String] = (Set.empty[String] ++ transpilerExtensions.map(m => m.value)).elements
    
    val transpiler = Template.transpiler(
      outputPaths,
      (((if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aepNames else ISZ[String]()) ++ appNames) :+ "Main").map(s => s"${basePackage}.${s}"),
      ISZ(s"art.ArtNative=${basePackage}.ArtNix", s"${basePackage}.Platform=${basePackage}.PlatformNix"),
      arsitOptions.bitWidth,
      maxArraySize,
      arsitOptions.maxStringSize,
      customSequenceSizes,
      customConstants,
      maxStackSize,
      _extensions,
      excludes,
      buildApps,
      additionalInstructions
    )

    addExeResource(dirs.binDir, ISZ(transpileScriptName), transpiler, T)
  }

  object Template {
    // @formatter:off
    @pure def portDef(portName: String,
                      portType: String): ST =
      return st"""$portName: Option[$portType]"""

    @pure def portOpt(portId: String,
                      portIdOpt: String,
                      portType: String,
                      mutable: B): ST = {
      if (mutable) {
        return st"""var ${portDef(portIdOpt, portType)} = None[$portType]()"""
      } else {
        return st"""val ${portDef(portIdOpt, portType)} = Some(${portId})"""
      }
    }
    @pure def portId(portIdName: String,
                     archPortInstanceName: String): ST =
      return st"""val $portIdName = Arch.${archPortInstanceName}.id"""

    @pure def portOptReset(portOptName: String,
                          portType: String): ST =
      return st"""$portOptName = None[$portType]()"""

    @pure def aepPortCase(portIdName: String,
                          portOptName: String,
                          payloadType: DataTypeNames,
                          isData: B): ST =
      return st"""case `$portIdName` =>
                 |  $portOptName = Some(d.asInstanceOf[${payloadType.qualifiedPayloadName}]${if(!payloadType.isEmptyType()) ".value" else ""})
                 |  ${if(!isData) "eventArrived()" else ""}"""

    @pure def AEPPayload(AEPPayloadTypeName: String,
                         portOptNames: ISZ[String]): ST =
      return st"""${AEPPayloadTypeName}(${(portOptNames, ", ")})"""

    @pure def appCases(portOptName: String,
                       portId: String,
                       payloadType: DataTypeNames): ST =
      return st"""${portOptName} match {
                 |  case Some(v) => ArtNix.updateData(${portId}, ${if(payloadType.isEmptyType()) "v" else s"${payloadType.qualifiedPayloadName}(v)"})
                 |  case _ =>
                 |}"""

    @pure def platformPortDecl(portName: String,
                               portId: Z) : ST =
      return st"""val $portName: Art.PortId = $portId"""

    @pure def platformPayload(payloadName: String,
                              fields: ISZ[ST]): ST = {
      return st"""@datatype class $payloadName(
                 |  ${(fields, ",\n")}
                 |) extends DataContent"""
    }

    @pure def artNixCases(srcArchPortId: String,
                         cases: ISZ[ST]): ST =
      return st"""r(Arch.$srcArchPortId.id) = ISZ(
                 |  ${(cases, ",\n")}
                 |)"""

    @pure def artNixCase(aepPortId: String,
                         dstArchPortId: String): ST =
      return st"(IPCPorts.$aepPortId, Arch.$dstArchPortId.id)"

    @pure def mainSend(portId: String): ST = {
      val isSM = arsitOptions.ipc == Cli.IpcMechanism.SharedMemory
      return st"""Platform.send${if (isSM) "Async" else ""}(IPCPorts.${portId}, IPCPorts.${if(isSM) s"$portId" else "Main"}, empty)"""
    }

    @pure def aep(packageName: String,
                  objectName: String,
                  portOpts: ISZ[ST],
                  portIds: ISZ[ST],
                  portOptResets: ISZ[ST],
                  portCases: ISZ[ST],
                  AEP_Id: String,
                  IPCPort_Id: String,
                  AEP_payload: ST,
                  isPeriodic: B): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object ${objectName} extends App {
                 |
                 |  ${if(!isPeriodic) "var state: AEPState.Type = AEPState.Start" else ""}
                 |  ${(portOpts, "\n")}
                 |
                 |  def main(args: ISZ[String]): Z = {
                 |
                 |    val seed: Z = if (args.size == z"1") {
                 |      val n = Z(args(0)).get
                 |      if (n == z"0") 1 else n
                 |    } else {
                 |      1
                 |    }
                 |
                 |    Platform.initialise(seed, Some(IPCPorts.${AEP_Id}))
                 |
                 |    val anyPortOpt: Option[art.Art.PortId] = None()
                 |    ${(portIds, "\n")}
                 |
                 |    // wait for first ping which should indicate all apps are past 
                 |    // their IPC init phase
                 |    Platform.receive(Some(IPCPorts.Main))
                 |    
                 |    // wait for second ping which should indicate all apps have completed
                 |    // their initialise entrypoint
                 |    Platform.receive(Some(IPCPorts.Main))
                 |
                 |    while (true) {
                 |      val (port, d) = Platform.receive(anyPortOpt)
                 |      port match {
                 |        ${(portCases, "\n")}
                 |        case IPCPorts.${IPCPort_Id} =>
                 |          ${if(isPeriodic) "sendEvent()" else "requested()"}
                 |        case _ => halt(s"Infeasible: ${"$port"}")
                 |      }
                 |    }
                 |
                 |    return 0
                 |  }
                 |  ${if(!isPeriodic)
                        st"""
                            |def eventArrived(): Unit = {
                            |  if (state == AEPState.EventRequested) {
                            |    sendEvent()
                            |  } else {
                            |    state = AEPState.EventArrived
                            |  }
                            |}
                            |
                            |def requested(): Unit = {
                            |  if (state == AEPState.EventArrived) {
                            |    sendEvent()
                            |  } else {
                            |    state = AEPState.EventRequested
                            |  }
                            |}
                            |""".render.toString
                       else ""}
                 |  def sendEvent(): Unit = {
                 |    Platform.send(IPCPorts.${IPCPort_Id}, IPCPorts.${AEP_Id},
                 |                  ${AEP_payload})
                 |
                 |    ${(portOptResets, "\n")}
                 |    ${if(!isPeriodic) "state = AEPState.Start" else ""}
                 |  }
                 |
                 |  override def atExit(): Unit = {
                 |    Platform.finalise()
                 |  }
                 |}"""

    }

    @pure def app(packageName: String,
                  objectName: String,
                  IPCPort_Id: String,
                  AEP_Id: String,
                  period: Z,
                  bridge: String,
                  AEP_Payload: ST,
                  portIds: ISZ[ST],
                  cases: ISZ[ST],
                  component: Component,
                  isPeriodic: B
                 ) : ST = {
      // @formatter:off
      val isSharedMemory = arsitOptions.ipc == Cli.IpcMechanism.SharedMemory
      def portId(p: Port) = s"${p.name}PortId"
      def portIdOpt(p: Port) = s"${portId(p)}Opt"
      
      val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)
      val inPorts = Util.getFeatureEnds(component.features).filter(p => CommonUtil.isInPort(p)).map(f => {
        val portName = CommonUtil.getLastName(f.identifier)
        val isTrigger = if(dispatchTriggers.isEmpty) T else {
          dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
        }
        Util.getPort(f, component, types, basePackage, isTrigger, z"-1000")
      })

      var globals: ISZ[ST] = ISZ(st"""val entryPoints: Bridge.EntryPoints = Arch.${bridge}.entryPoints
                                     |val appPortId: Art.PortId = IPCPorts.${IPCPort_Id}
                                     |val appPortIdOpt: Option[Art.PortId] = Some(appPortId)""")

      var inits: ISZ[ST] = ISZ(st"Platform.initialise(seed, appPortIdOpt)")

      if(!isSharedMemory && portIds.nonEmpty) {
        globals = globals :+ st"""val empty: art.Empty = art.Empty()
                                 |val aepPortId: Art.PortId = IPCPorts.${AEP_Id}
                                 |val aepPortIdOpt: Option[Art.PortId] = Some(aepPortId)"""
      }

      for(p <- inPorts) {
        globals = globals :+ st"val ${portId(p)}: Art.PortId = Arch.${bridge}.${p.name}.id" 
        
        if(isSharedMemory) {
          globals = globals :+ st"val ${portIdOpt(p)}: Option[Art.PortId] = Some(${portId(p)})"
          inits = inits :+ st"Platform.initialise(seed, ${portIdOpt(p)})"
        }
      }

      var computeBody: ST = st""

      // @formatter:off
      val body = {
        if(isSharedMemory) {
          /********** SHARED MEMORY BODY **********/

          val loopBody = {

            val receiveOnInPorts =
              for(p <- inPorts) yield {
                val dispatch = if(CommonUtil.isAadlDataPort(p.feature)) { "F" } else { "T" }
                st"""Platform.receiveAsync(${portIdOpt(p)}) match {
                    |  case Some((_, v: ${p.getPortTypeNames.qualifiedPayloadName})) => ArtNix.updateData(${portId(p)}, v)${ if(!isPeriodic) s"; dispatch = ${dispatch}" else "" }
                    |  case Some((_, v)) => halt(s"Unexpected payload on port ${p.name}.  Expecting something of type ${p.getPortTypeNames.qualifiedPayloadName} but received $${v}")
                    |  case None() => // do nothing
                    |}"""
              }

            if(isPeriodic)
              st"""${(receiveOnInPorts, "\n")}
                  |entryPoints.compute()
                  |Process.sleep($period)"""
            else {

              st"""var dispatch = F
                  |${(receiveOnInPorts, "\n")}
                  |if (dispatch) {
                  |  entryPoints.compute()
                  |  Process.sleep($period)
                  |} else {
                  |  Process.sleep(10)
                  |}"""
            }
          }

          computeBody = loopBody

          st"""var terminated = F
              |while (!terminated) {
              |  val termOpt = Platform.receiveAsync(appPortIdOpt)
              |  if (termOpt.isEmpty) {
              |    compute()
              |  } else {
              |    terminated = T
              |  }
              |}
              |exit()"""
        } else {
          val optSleep: Option[String] = if(isPeriodic) Some(s"Process.sleep($period)") else None()
          val optReceives: Option[ST] = if(portIds.nonEmpty) { 
            Some(st"""Platform.send(aepPortId, appPortId, empty)
                       |val (_, d) = Platform.receive(aepPortIdOpt)
                       |val ${AEP_Payload} = d
                       |${(cases, "\n")}""")
          } else None()

          computeBody = st"""${optSleep}
                            |${optReceives}
                            |entryPoints.compute()"""
  
          /******* MESSAGE QUEUE BODY *********/
          st"""while (true) {
              |  compute()
              |}"""
        }
      }

      return st"""// #Sireum

                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object ${objectName} extends App {
                 |
                 |  ${(globals, "\n")}
                 |
                 |  def initialiseArchitecture(seed: Z): Unit = {
                 |    ${(inits, "\n")}
                 |
                 |    Art.run(Arch.ad)
                 |  }
                 |
                 |  def initialise(): Unit = {
                 |    entryPoints.initialise()
                 |  }
                 |  
                 |  def compute(): Unit = {
                 |    ${computeBody}
                 |  }
                 |
                 |  def finalise(): Unit = {
                 |    entryPoints.finalise()
                 |  }
                 |  
                 |  def main(args: ISZ[String]): Z = {
                 |
                 |    val seed: Z = if (args.size == z"1") {
                 |      val n = Z(args(0)).get
                 |      if (n == z"0") 1 else n
                 |    } else {
                 |      1
                 |    }
                 |
                 |    initialiseArchitecture(seed)
                 |
                 |    Platform.receive(${if(isSharedMemory) "appPortIdOpt" else "Some(IPCPorts.Main)"}) // pause after setting up component
                 |
                 |    initialise()
                 |
                 |    Platform.receive(${if(isSharedMemory) "appPortIdOpt" else "Some(IPCPorts.Main)"}) // pause after component init
                 |
                 |    println("${objectName} starting ...")
                 |
                 |    ${if(isPeriodic) "ArtNix.timeDispatch()" else "ArtNix.eventDispatch()"}
                 |
                 |    ${body}
                 |
                 |    return 0
                 |  }
                 |
                 |  def exit(): Unit = {
                 |    finalise()
                 |    Platform.finalise()
                 |  }
                 |
                 |  override def atExit(): Unit = {
                 |    exit()
                 |  }
                 |}"""
      // @formatter:on
    }

    @pure def ipc(packageName: String,
                  ports: ISZ[ST],
                  payloads: ISZ[ST]): ST = {
      val aep = if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue)
        st"""
            |@enum object AEPState {
            |  'Start
            |  'EventArrived
            |  'EventRequested
            |}
            |
            |${(payloads, "\n\n")}"""
        else st""""""

      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object IPCPorts {
                 |  ${(ports, "\n")}
                 |}
                 |${aep}
                 |"""
    }

    @pure def artNix(packageName: String,
                     cases: ISZ[ST],
                     eventInPorts: ISZ[String]): ST = {
      val isSharedMemory = arsitOptions.ipc == Cli.IpcMechanism.SharedMemory
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object ArtNix {
                 |
                 |  val maxPortIds: Art.PortId = IPCPorts.Main + 1
                 |  val timeTriggered: TimeTriggered = TimeTriggered()
                 |  val noData: Option[DataContent] = None()
                 |  val data: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
                 |  val connection: MS[Art.PortId, ISZ[(Art.PortId, Art.PortId)]] = {
                 |    val r = MS.create[Art.PortId, ISZ[(Art.PortId, Art.PortId)]](maxPortIds, ISZ())
                 |
                 |    ${(cases, "\n")}
                 |
                 |    r
                 |  }
                 |  val eventInPorts: MS[Z, Art.PortId] = MSZ(
                 |    ${(eventInPorts, ",\n")}
                 |  )
                 |  var frozen: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
                 |  var outgoing: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, noData)
                 |  var isTimeDispatch: B = F
                 |
                 |  def updateData(port: Art.PortId, d: DataContent): Unit = {
                 |    data(port) = Some(d)
                 |  }
                 |
                 |  def timeDispatch(): Unit = {
                 |    isTimeDispatch = T
                 |  }
                 |
                 |  def eventDispatch(): Unit = {
                 |    isTimeDispatch = F
                 |  }
                 |
                 |  def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
                 |    if (isTimeDispatch) {
                 |      return timeTriggered
                 |    } else {
                 |      var r = ISZ[Art.PortId]()
                 |      for (i <- eventInPorts if data(i).nonEmpty) {
                 |        r = r :+ i
                 |      }
                 |      return EventTriggered(r)
                 |    }
                 |  }
                 |
                 |  def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                 |    frozen = data
                 |    for (i <- eventPortIds) {
                 |      data(i) = noData
                 |    }
                 |  }
                 |
                 |  def putValue(portId: Art.PortId, data: DataContent): Unit = {
                 |    outgoing(portId) = Some(data)
                 |  }
                 |
                 |  def getValue(portId: Art.PortId): Option[DataContent] = {
                 |    return frozen(portId)
                 |  }
                 |
                 |  def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
                 |    for (p <- dataPortIds) {
                 |      outgoing(p) match {
                 |        case Some(d) =>
                 |          outgoing(p) = noData
                 |          for(e <- connection(p)){
                 |            Platform.send${if(isSharedMemory)"Async" else ""}(e._1, e._2, d)
                 |          }
                 |        case _ =>
                 |      }
                 |    }
                 |
                 |    for (p <- eventPortIds) {
                 |      outgoing(p) match {
                 |        case Some(d) =>
                 |          outgoing(p) = noData
                 |          for(e <- connection(p)){
                 |            Platform.send${if(isSharedMemory)"Async" else ""}(e._1, e._2, d)
                 |          }
                 |        case _ =>
                 |      }
                 |    }
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
                 |  def run(): Unit = {
                 |  }
                 |
                 |  def time(): Art.Time = {
                 |    return Process.time()
                 |  }
                 |}
                 |"""
    }

    @pure def main(packageName: String,
                   sends: ISZ[ST]): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object Main extends App {
                 |  def main(args: ISZ[String]): Z = {
                 |
                 |    val seed: Z = if (args.size == z"1") {
                 |      val n = Z(args(0)).get
                 |      if (n == z"0") 1 else n
                 |    } else {
                 |      1
                 |    }
                 |
                 |    Platform.initialise(seed, None())
                 |
                 |    val empty = art.Empty()
                 |
                 |    ${(sends, "\n")}
                 |
                 |    Platform.finalise()
                 |    return 0
                 |  }
                 |}
                 | """
    }

    @pure def MessageQueue(packageName: String): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@ext object MessageQueue {
                 |  def create(msgid: Z): Z = $$
                 |  def get(msgid: Z): Z = $$
                 |  def send(msgid: Z, port: Art.PortId, d: DataContent): Unit = $$
                 |  def receive(): (Art.PortId, DataContent) = $$
                 |  def sendAsync(msgid: Z, port: Art.PortId, d: DataContent): B = $$
                 |  def receiveAsync(): Option[(Art.PortId, DataContent)] = $$
                 |  def remove(msgid: Z): Unit = $$
                 |}
                 |"""
    }

    @pure def MessageQueueExt(packageName: String): ST = {
      return st"""package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object MessageQueue_Ext {
                 |  def create(msgid: Z): Z = halt("stub")
                 |  def get(msgid: Z): Z = halt("stub")
                 |  def send(msgid: Z, port: Art.PortId, d: DataContent): Unit = halt("stub")
                 |  def receive(): (Art.PortId, DataContent) = halt("stub")
                 |  def sendAsync(msgid: Z, port: Art.PortId, d: DataContent): B = halt("stub")
                 |  def receiveAsync(): Option[(Art.PortId, DataContent)] = halt("stub")
                 |  def remove(msgid: Z): Unit = halt("stub")
                 |}
                 |"""
    }

    @pure def SharedMemory(packageName: String): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |// This file was auto-generated.  Do not edit
                 |
                 |@ext object SharedMemory {
                 |  def create(id: Z): Z = $$
                 |  def get(id: Z): Z = $$
                 |  def send(id: Z, port: Art.PortId, d: DataContent): Unit = $$
                 |  def receive(port: Art.PortId): DataContent = $$
                 |  def sendAsync(id: Z, port: Art.PortId, d: DataContent): B = $$
                 |  def receiveAsync(port: Art.PortId): Option[DataContent] = $$
                 |  def remove(id: Z): Unit = $$
                 |}"""
    }

    @pure def SharedMemory_Ext(packageName: String): ST = {
      return st"""package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |// This file was auto-generated.  Do not edit
                 |
                 |object SharedMemory_Ext {
                 |  def create(id: Z): Z = halt("stub")
                 |  def get(id: Z): Z = halt("stub")
                 |  def send(id: Z, port: Art.PortId, d: DataContent): Unit = halt("stub")
                 |  def receive(port: Art.PortId): DataContent = halt("stub")
                 |  def sendAsync(id: Z, port: Art.PortId, d: DataContent): B = halt("stub")
                 |  def receiveAsync(port: Art.PortId): Option[DataContent] = halt("stub")
                 |  def remove(id: Z): Unit = halt("stub")
                 |}"""
    }

    @pure def platform(packageName: String): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@ext object Platform {
                 |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = $$
                 |  def receive(portOpt: Option[Art.PortId]): (Art.PortId, DataContent) = $$
                 |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = $$
                 |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = $$
                 |  def receiveAsync(portOpt: Option[Art.PortId]): Option[(Art.PortId, DataContent)] = $$
                 |  def finalise(): Unit = $$
                 |}
                 |"""
    }

    @pure def PlatformExt(packageName: String): ST = {
      return st"""package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object Platform_Ext {
                 |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = halt("stub")
                 |  def receive(portOpt: Option[Art.PortId]): (Art.PortId, DataContent) = halt("stub")
                 |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = halt("stub")
                 |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = halt("stub")
                 |  def receiveAsync(portOpt: Option[Art.PortId]): Option[(Art.PortId, DataContent)] = halt("stub")
                 |  def finalise(): Unit = halt("stub")
                 |}
                 |"""
    }

    @pure def PlatformNix(packageName: String): ST = {
      val isSM: B = arsitOptions.ipc == Cli.IpcMechanism.SharedMemory

      val init =
        if(isSM) st"""val id = seed + port
                    |SharedMemory.create(id)
                    |ids = ids :+ id"""
        else  st"""val msgid = MessageQueue.create(seed + port)
                  |msgidOpt = Some(msgid)"""

      val receive =
        if(isSM) st"""portOpt match {
                     |  case Some(port) =>
                     |    val d = SharedMemory.receive(seed + port)
                     |    return (port, d)
                     |  case _ => halt("Unsupported receive operation without port.")
                     |}"""
        else st"""val p = MessageQueue.receive()
                 |portOpt match {
                 |  case Some(port) => assert(p._1 == port)
                 |  case _ =>
                 |}
                 |return p"""

      val send =
        if(isSM) "SharedMemory.send(port, seed + port, data)"
        else "MessageQueue.send(seed + app, port, data)"

      val sendAsync =
        if(isSM) "SharedMemory.sendAsync(port, seed + port, data)"
        else "MessageQueue.sendAsync(seed + app, port, data)"

      val finalise =
        if(isSM) st"""for (id <- ids) {
                     |  SharedMemory.remove(id)
                     |}"""
        else st"""msgidOpt match {
                  |  case Some(msgid) => MessageQueue.remove(msgid)
                  |  case _ =>
                  |}"""

      val receiveAsync =
        if(isSM) st"""portOpt match {
                     |  case Some(port) =>
                     |    val dOpt = SharedMemory.receiveAsync(seed + port)
                     |    dOpt match {
                     |      case Some(d) => return Some((port, d))
                     |      case _ => return None()
                     |    }
                     |  case _ => halt("Unsupported receive operation without port.")
                     |}"""
        else st"""val pOpt = MessageQueue.receiveAsync()
                  |(portOpt, pOpt) match {
                  |  case (Some(port), Some((p, _))) => assert(p == port); return pOpt
                  |  case _ => return None()
                  |}"""

      return st"""// #Sireum
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object PlatformNix {
                 |
                 |  var seed: Z = 0
                 |  ${if(isSM) "var ids: ISZ[Z] = ISZ()"
                      else "var msgidOpt: Option[Z] = None()"}
                 |
                 |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = {
                 |    PlatformNix.seed = seed
                 |    portOpt match {
                 |      case Some(port) =>
                 |        $init
                 |      case _ =>
                 |    }
                 |  }
                 |
                 |  def receive(portOpt: Option[Art.PortId]): (Art.PortId, DataContent) = {
                 |    ${receive}
                 |  }
                 |
                 |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = {
                 |    ${send}
                 |  }
                 |
                 |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = {
                 |    val r = ${sendAsync}
                 |    return r
                 |  }
                 |
                 |  def receiveAsync(portOpt: Option[Art.PortId]): Option[(Art.PortId, DataContent)] = {
                 |    ${receiveAsync}
                 |  }
                 |
                 |  def finalise(): Unit = {
                 |    ${finalise}
                 |  }
                 |}
                 |"""
    }

    @pure def Process(packageName: String): ST = {
      return st"""// #Sireum
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art.Art
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@ext object Process {
                 |  def sleep(n: Z): Unit = $$
                 |
                 |  def time(): Art.Time = $$
                 |}
                 |"""
    }

    @pure def ProcessExt(packageName: String): ST = {
      return st"""package $packageName
                 |
                 |import org.sireum._
                 |import art.Art
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object Process_Ext {
                 |  def sleep(millis: Z): Unit = halt("stub")
                 |
                 |  def time(): Art.Time = halt("stub")
                 |}"""
    }
    
    @pure def compile(arch: Cli.ArsitPlatform.Type): ST = {

      val script_home = "${SCRIPT_HOME}"
      val cOutputDirRel = SlangUtil.relativizePaths(dirs.binDir, cOutputDir, script_home)

      val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
      val mv: ST = if(arch == Cli.ArsitPlatform.Cygwin) {
        st"mv *.exe ${script_home}/${buildDir}/"
      } else {
        st"""mv *_App ${script_home}/${buildDir}/
            |${if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) st"mv *_AEP ${script_home}/${buildDir}/" else ""}
            |mv Main ${script_home}/${buildDir}/"""
      }
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd ${script_home}
          |mkdir -p ${buildDir}
          |mkdir -p ${cOutputDirRel}/${buildDir}
          |cd ${cOutputDirRel}/${buildDir}
          |cmake -DCMAKE_BUILD_TYPE=Release ..
          |make $$MAKE_ARGS
          |$mv"""
    }

    @pure def run(aeps: ISZ[String],
                  apps: ISZ[String],
                  arch: Cli.ArsitPlatform.Type): ST = {

      val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
      val ext = if(arch.toString == "win") ".exe" else ""
      val staep = if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aeps.map(st => st"""${buildDir}/$st$ext 2> /dev/null &""" ) else ISZ()
      val stapp = apps.map(st => {
        val prefix = arch match {
          case Cli.ArsitPlatform.Cygwin => "cygstart mintty /bin/bash"
          case Cli.ArsitPlatform.Linux => "x-terminal-emulator -e sh -c"
          case Cli.ArsitPlatform.MacOS => "open -a Terminal"
          case _ => halt(s"Unexpected platform ${arch}")
        }
        st"""$prefix "${buildDir}/${st}$ext" &""" })
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |${(staep, "\n")}
          |${(stapp, "\n")}
          |read -p "Press enter to initialise components ..."
          |${buildDir}/Main$ext
          |read -p "Press enter again to start ..."
          |${buildDir}/Main$ext"""
    }

    @pure def stop(apps: ISZ[String]) : ST = {
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |APPS="${(apps, " ")}"
          |for APP in $${APPS}; do
          |  pkill -f $$APP
          |  pkill -9 -f $$APP
          |done
          |ME=`whoami`
          |
          |# message queue
          |IPCS_Q=`ipcs -q | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -q $$id;
          |done
          |
          |# shared memory
          |IPCS_Q=`ipcs -m | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -m $$id;
          |done
          |
          |# semaphores
          |IPCS_Q=`ipcs -s | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -s $$id;
          |done
          |"""
    }


    @pure def transpiler(sourcepaths: ISZ[String],
                         apps: ISZ[String],
                         forwards: ISZ[String],
                         numBits: Z,
                         maxSequenceSize: Z,
                         maxStringSize: Z,
                         customArraySizes: ISZ[String],
                         customConstants: ISZ[String],
                         stackSizeInBytes: Z,
                         extensions: ISZ[String],
                         excludes: ISZ[String],
                         buildApps: B,
                         additionalInstructions: Option[ST]): ST = {

      val _stackSizeInBytes: String = if(stackSizeInBytes < 0) {
        "16*1024*1024" // default set in org.sireum.transpilers.cli.cTranspiler
      } else {
        stackSizeInBytes.string
      }
      
      val ops = CTranspilerOption(
          sourcepath = ISZ(dirs.srcMainDir),
          output = Some(cOutputDir),
          verbose = T,
          projectName = Some("main"),  // default set in org.sireum.transpilers.cli.cTranspiler
          apps = apps,
          unroll = F, // default set in org.sireum.transpilers.cli.cTranspiler
          fingerprint = 3, // default set in org.sireum.transpilers.cli.cTranspiler
          bitWidth = numBits,
          maxStringSize = maxStringSize,
          maxArraySize = maxSequenceSize,
          customArraySizes = customArraySizes,
          customConstants = customConstants,
          plugins = ISZ(),
          exts = extensions,
          forwarding = forwards,
          stackSize = Some(_stackSizeInBytes),
          excludeBuild = excludes,
          libOnly = !buildApps,
          stableTypeId = T,
          save = None(),
          load = None()
      )

      transpilerOptions = ISZ(ops)
      return transpilerX(ops, additionalInstructions)
    }

    @pure def transpilerX(opts: CTranspilerOption,
                          additionalInstructions: Option[ST]): ST = {

      val script_home = s"$${${Util.SCRIPT_HOME}}"

      val projHomesRel = opts.sourcepath.map(s => SlangUtil.relativizePaths(dirs.binDir, s, script_home))
      val cOutputDirRel = SlangUtil.relativizePaths(dirs.binDir, opts.output.get, script_home)

      val path_sep = s"$${PATH_SEP}"

      var ret = st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |
          |if [ -z "$${SIREUM_HOME}" ]; then
          |  echo "SIREUM_HOME not set. Refer to https://github.com/sireum/kekinian/#installing"
          |  exit 1
          |fi
          |
          |PATH_SEP=":"
          |if [ -n "$$COMSPEC" -a -x "$$COMSPEC" ]; then
          |  PATH_SEP=";"
          |fi
          |
          |${Util.SCRIPT_HOME}=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |OUTPUT_DIR="${cOutputDirRel}"
          |
          |$${SIREUM_HOME}/bin/sireum slang transpilers c \
          |  --sourcepath "${(projHomesRel, path_sep)}" \
          |  --output-dir "$${OUTPUT_DIR}" \
          |  --name "${opts.projectName.get}" \
          |  --apps "${(opts.apps, ",")}" \
          |  --fingerprint ${opts.fingerprint} \
          |  --bits ${opts.bitWidth} \
          |  --string-size ${opts.maxStringSize} \
          |  --sequence-size ${opts.maxArraySize} \
          |  --sequence "${(opts.customArraySizes, ";")}" \
          |  --constants "${(opts.customConstants, ";")}" \
          |  --forward "${(opts.forwarding, ",")}" \
          |  --stack-size "${opts.stackSize.get}" \
          |  --stable-type-id"""

      var extras: ISZ[ST] = ISZ()

      if(opts.exts.nonEmpty) {
        val extsRel = opts.exts.map(s => SlangUtil.relativizePaths(dirs.binDir, s, script_home))
        extras = extras :+ st""" \
                               |  --exts "${(extsRel, path_sep)}""""
      }

      if(opts.excludeBuild.nonEmpty) {
        extras = extras :+ st""" \
                               |  --exclude-build "${(opts.excludeBuild, ",")}""""
      }

      if(opts.libOnly) {
        extras = extras :+ st""" \
                               |  --lib-only"""
      }

      if(opts.verbose) {
        extras = extras :+ st""" \
                               |  --verbose"""
      }

      if(additionalInstructions.nonEmpty) {
        extras = extras :+ st"""
                               |
                               |${additionalInstructions}"""
      }

      return st"""${ret}${(extras, "")}"""
    }
  }
  // @formatter:on
}
