package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.cli.Cli.{ArsitOption, Ipcmech}
import org.sireum.util.MMap

class ArtNixGen {
  var projOutputDir: File = _
  var nixOutputDir : File = _
  var cOutputDir : File = _
  var binOutputDir: File = _

  var arsitOptions: ArsitOption = _

  var basePackage: String = _

  val componentMap : MMap[String, Component] = org.sireum.util.mmapEmpty

  var portId: Z = _
  def getPortId(): Z = {
    val r = portId
    portId = portId + 1
    return r
  }

  def generator(outputDir: File, m: Aadl, topPackageName: String, nextPortId: Z, nextComponentId: Z, o: ArsitOption): Z = {
    basePackage = Util.sanitizeName(topPackageName)
    this.projOutputDir = outputDir // where the slang-embedded code was generated

    binOutputDir = new File(projOutputDir, "../../bin")
    cOutputDir = new File(projOutputDir, "../c")
    nixOutputDir = new File(projOutputDir, "nix")
    portId = nextPortId

    arsitOptions = o

    gen(m)

    portId
  }

  def gen(model: Aadl): Unit = {

    var connections: ISZ[ConnectionInstance] = ISZ()

    { // build component map
      def r(c: Component): Unit = {
        assert(!componentMap.contains(Util.getName(c.identifier)))
        componentMap += (Util.getName(c.identifier) → c)
        connections = connections ++ c.connectionInstances
        for (s <- c.subComponents) r(s)
      }
      for (c <- model.components) r(c)
    }

    var platformPorts: ISZ[ST] = ISZ()
    var platformPayloads: ISZ[ST] = ISZ()
    var mainSends: ISZ[ST] = ISZ()
    var inPorts: ISZ[Port] = ISZ()
    var aepNames: ISZ[String] = ISZ()
    var appNames: ISZ[String] = ISZ()

    val components = componentMap.filter(p =>
      p._2.category == ComponentCategory.Thread || p._2.category == ComponentCategory.Device)

    for ((archVarName, m) <- components) {

      val isPeriodic: B = {
        Util.getDiscreetPropertyValue[ValueProp](m.properties, Util.Prop_DispatchProtocol) match {
          case Some(x) =>
            x.value.toString match {
              case "Sporadic" => F
              case "Periodic" => T
            }
          case _ =>
            if (m.category == ComponentCategory.Device) T
            else ???
        }
      }

      val name: Names = Util.getNamesFromClassifier(m.classifier.get, basePackage)

      val bridgeInstanceVarName: String = Util.getName(m.identifier)
      val AEP_Id: String = s"${name.component}_AEP"
      val App_Id: String = s"${name.component}_App"

      val AEPPayloadTypeName: String = s"${AEP_Id}_Payload"

      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portIdOpts: ISZ[ST] = ISZ()

      var portOptResets: ISZ[ST] = ISZ()
      var aepPortCases: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      for (p <- Util.getFeatureEnds(m.features) if Util.isInFeature(p)) {
        assert (Util.isPort(p))
        val port = Port(p, m, basePackage)
        val portIdName: String = port.name + "PortId"
        val portOptName: String = port.name + "Opt"
        val portType: String = port.portType.qualifiedReferencedTypeName
        val portPayloadTypeName: String = port.portType.qualifiedPayloadName
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${port.name}"

        portDefs = portDefs :+ Template.portDef(portOptName, portType)
        portOpts = portOpts :+ Template.portOpt("", portOptName, portType, T)

        portIds :+= Template.portId(portIdName, archPortInstanceName)
        portIdOpts :+= Template.portOpt(portIdName, portOptName, "Art.PortId", F)

        aepPortCases = aepPortCases :+ Template.aepPortCase(portIdName, portOptName, portPayloadTypeName, Util.isDataPort(p))
        portOptResets = portOptResets :+ Template.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ Template.appCases(portOptName, portIdName, portPayloadTypeName)

        inPorts :+= port
      }

      platformPorts = platformPorts :+ Template.platformPortDecl(App_Id, getPortId())
      if(arsitOptions.ipc == Ipcmech.Message_queue) {
        platformPorts :+= Template.platformPortDecl(AEP_Id, getPortId())
      }

      if(portOpts.nonEmpty) {
        platformPayloads = platformPayloads :+
          Template.platformPayload(AEPPayloadTypeName, portDefs)
      }

      if(portOpts.nonEmpty)
        aepNames = aepNames :+ AEP_Id
      appNames = appNames :+ App_Id

      mainSends :+= Template.mainSend(App_Id)
      if(arsitOptions.ipc == Ipcmech.Message_queue) {
        mainSends :+= Template.mainSend(AEP_Id)
      }

      val AEP_Payload = Template.AEPPayload(AEPPayloadTypeName, portOptNames)
      if(portOpts.nonEmpty && arsitOptions.ipc == Ipcmech.Message_queue) {
        val stAep = Template.aep(basePackage, AEP_Id, portOpts,
          portIds, portOptResets, aepPortCases, AEP_Id, App_Id, AEP_Payload, isPeriodic)
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/${AEP_Id}.scala"), stAep)
      }

      val stApp = Template.app(basePackage, App_Id, App_Id,
        AEP_Id, Util.getPeriod(m), bridgeInstanceVarName, AEP_Payload, portIds, appCases, m, isPeriodic)
      Util.writeFile(new File(nixOutputDir, s"${basePackage}/${App_Id}.scala"), stApp)
    }

    var artNixCases: ISZ[ST] = ISZ()
    for(c <- connections) {
      val dstComp = componentMap(Util.getName(c.dst.component))
      val srcComp = componentMap(Util.getName(c.src.component))

      if((Util.isDevice(srcComp) || Util.isThread(srcComp)) & (Util.isDevice(dstComp) || Util.isThread(dstComp))) {
        val dstPath = Util.getName(c.dst.feature)
        val dstArchPortInstanceName =
          s"${Util.getName(dstComp.identifier)}.${Util.getLastName(c.dst.feature)}"
        val name: Names = Util.getNamesFromClassifier(dstComp.classifier.get, basePackage)

        val srcArchPortInstanceName =
          s"${Util.getName(srcComp.identifier)}.${Util.getLastName(c.src.feature)}"

        if (inPorts.map(_.path).elements.contains(dstPath) &&
          (Util.isThread(srcComp) || Util.isDevice(srcComp)) && (Util.isThread(dstComp) || Util.isDevice(dstComp))) {
          val dstComp = if (arsitOptions.ipc == Ipcmech.Shared_memory) s"${name.component}_App" else s"${name.component}_AEP"
          artNixCases = artNixCases :+
            Template.artNixCase(srcArchPortInstanceName, dstArchPortInstanceName, dstComp)
        }
      } else {
        println(s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${Util.getName(c.name)}")
      }
    }

    platformPorts = platformPorts :+ Template.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case Ipcmech.Message_queue =>

        Util.writeFile(new File(nixOutputDir, s"${basePackage}/MessageQueue.scala"), Template.MessageQueue(basePackage))
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/MessageQueue_Ext.scala"), Template.MessageQueueExt(basePackage))

      case Ipcmech.Shared_memory =>
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/SharedMemory.scala"), Template.SharedMemory(basePackage))
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/SharedMemory_Ext.scala"), Template.SharedMemory_Ext(basePackage))
    }

    val stIPC = Template.ipc(basePackage, platformPorts, platformPayloads)
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/IPC.scala"), stIPC)

    val stArtNix = Template.artNix(basePackage, artNixCases,
      inPorts.withFilter(p => Util.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id"))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/ArtNix.scala"), stArtNix)

    val stMain = Template.main(basePackage, mainSends)
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Main.scala"), stMain)

    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Platform.scala"), Template.platform(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Platform_Ext.scala"), Template.PlatformExt(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/PlatformNix.scala"), Template.PlatformNix(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Process.scala"), Template.Process(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Process_Ext.scala"), Template.ProcessExt(basePackage))

    Util.writeFile(new File(binOutputDir, "compile-cygwin.sh"), Template.compile("win"))
    Util.writeFile(new File(binOutputDir, "compile-linux.sh"), Template.compile("linux"))
    Util.writeFile(new File(binOutputDir, "compile-mac.sh"), Template.compile("mac"))

    Util.writeFile(new File(binOutputDir, "run-cygwin.sh"), Template.run(aepNames, appNames, "win"))
    Util.writeFile(new File(binOutputDir, "run-linux.sh"), Template.run(aepNames, appNames, "linux"))
    Util.writeFile(new File(binOutputDir, "run-mac.sh"), Template.run(aepNames, appNames, "mac"))

    Util.writeFile(new File(binOutputDir, "stop.sh"), Template.stop(
      (if(arsitOptions.ipc == Ipcmech.Message_queue) aepNames else ISZ[String]()) ++ appNames))

    Util.writeFile(new File(cOutputDir, s"ext/ipc.c"), Util.getIpc(arsitOptions.ipc, basePackage))
    Util.writeFile(new File(cOutputDir, s"ext/ext.c"), st"""// add c extension code here""", false)

    var outputPaths: ISZ[String] = ISZ(projOutputDir.getAbsolutePath)
    if(!nixOutputDir.getAbsolutePath.contains(projOutputDir.getAbsolutePath))
      outputPaths = outputPaths :+ nixOutputDir.getAbsolutePath

    val tranpiler = Template.transpiler(
      outputPaths,
      (((if(arsitOptions.ipc == Ipcmech.Message_queue) aepNames else ISZ[String]()) ++ appNames) :+ "Main").map(s => s"${basePackage}.${s}"),
      ISZ(s"art.ArtNative=${basePackage}.ArtNix", s"${basePackage}.Platform=${basePackage}.PlatformNix"))
    Util.writeFile(new File(binOutputDir, "transpile.sh"), tranpiler)

    import scala.language.postfixOps
    import sys.process._
    s"chmod -R u+x $binOutputDir" !
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
                          payloadTypeName: String,
                          isData: B): ST =
      return st"""case `$portIdName` =>
                 |  $portOptName = Some(d.asInstanceOf[$payloadTypeName]${if(!Util.isEmptyType(payloadTypeName)) ".value" else ""})
                 |  ${if(!isData) "eventArrived()" else ""}"""

    @pure def AEPPayload(AEPPayloadTypeName: String,
                         portOptNames: ISZ[String]): ST =
      return st"""${AEPPayloadTypeName}(${(portOptNames, ", ")})"""

    @pure def appCases(portOptName: String,
                       portId: String,
                       payloadName: String): ST =
      return st"""${portOptName} match {
                 |  case Some(v) => ArtNix.updateData(${portId}, ${if(Util.isEmptyType(payloadName)) "v" else s"${payloadName}(v)"})
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

    @pure def artNixCase(srcArchPortId: String,
                         dstArchPortId: String,
                         aepPortId: String): ST =
      return st"""r(Arch.$srcArchPortId.id) =
                 |  (IPCPorts.$aepPortId, Arch.$dstArchPortId.id)"""

    @pure def mainSend(portId: String): ST = {
      val isSM = arsitOptions.ipc == Ipcmech.Shared_memory
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
                  period: String,
                  bridge: String,
                  AEP_Payload: ST,
                  portIds: ISZ[ST],
                  cases: ISZ[ST],
                  component: Component,
                  isPeriodic: B
                 ) : ST = {
      // @formatter:off
      val isSharedMemory = arsitOptions.ipc == Ipcmech.Shared_memory
      def portId(p: Port) = s"${p.name}PortId"
      def portIdOpt(p: Port) = s"${portId(p)}Opt"
      val inPorts = Util.getFeatureEnds(component.features).withFilter(p => Util.isPort(p) && Util.isInFeature(p)).map(Port(_, component, basePackage))

      val aepinit =
        if(!isSharedMemory && portIds.nonEmpty) {
          st"""val empty = art.Empty()
              |val aepPortId = IPCPorts.${AEP_Id}
              |val aepPortIdOpt: Option[Art.PortId] = Some(aepPortId)""".render
        } else ""

      val portSection =
        if(isSharedMemory) {
          for(p <- inPorts) yield {
            st"""val ${portId(p)} = Arch.${bridge}.${p.name}.id
                |val ${portIdOpt(p)} = Some(${portId(p)})
                |Platform.initialise(seed, ${portIdOpt(p)})"""
          }
        } else
          portIds

      // @formatter:off
      val body = {
        if(isSharedMemory) {
          /********** SHARED MEMORY BODY **********/

          val loopBody = {
            if(isPeriodic)
              st"""entryPoints.compute()
                  |Process.sleep($period)"""
            else {
              val receiveOnInPorts =
                for(p <- inPorts) yield {
                  st"""Platform.receiveAsync(${portIdOpt(p)}) match {
                      |  case Some((_, v: ${p.portType.qualifiedPayloadName})) => ArtNix.updateData(${portId(p)}, v); dispatch = T
                      |  case _ =>
                      |}"""
                }
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

          st"""var terminated = F
              |while (!terminated) {
              |  val termOpt = Platform.receiveAsync(appPortIdOpt)
              |  if (termOpt.isEmpty) {
              |    ${loopBody}
              |  } else {
              |    terminated = T
              |  }
              |}
              |exit()"""
        } else {
          /******* MESSAGE QUEUE BODY *********/
          st"""while (true) {
              |  ${if(isPeriodic) {
                   s"Process.sleep($period)"
                 } else ""}
              |  ${if(portIds.nonEmpty) {
                   st"""Platform.send(aepPortId, appPortId, empty)
                       |val (_, d) = Platform.receive(aepPortIdOpt)
                       |val ${AEP_Payload} = d
                       |${(cases, "\n")}""".render
                 } else ""}
              |  entryPoints.compute()
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
                 |  def main(args: ISZ[String]): Z = {
                 |
                 |    val seed: Z = if (args.size == z"1") {
                 |      val n = Z(args(0)).get
                 |      if (n == z"0") 1 else n
                 |    } else {
                 |      1
                 |    }
                 |
                 |    val entryPoints = Arch.${bridge}.entryPoints
                 |    ${aepinit}
                 |    val appPortId: Art.PortId = IPCPorts.${IPCPort_Id}
                 |    val appPortIdOpt: Option[Art.PortId] = Some(appPortId)
                 |    Platform.initialise(seed, appPortIdOpt)
                 |
                 |    ${(portSection, "\n\n")}
                 |    Art.run(Arch.ad)
                 |
                 |    entryPoints.initialise()
                 |
                 |    Platform.receive(${if(isSharedMemory) "appPortIdOpt" else "Some(IPCPorts.Main)"})
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
                 |    Arch.${bridge}.entryPoints.finalise()
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
      val aep = if(arsitOptions.ipc == Ipcmech.Message_queue)
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
      val isSharedMemory = arsitOptions.ipc == Ipcmech.Shared_memory
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
                 |  val data: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, None())
                 |  val noData: Option[DataContent] = None()
                 |  val connection: MS[Art.PortId, (Art.PortId, Art.PortId)] = {
                 |    val r = MS.create[Art.PortId, (Art.PortId, Art.PortId)](maxPortIds, (IPCPorts.Main, IPCPorts.Main))
                 |
                 |    ${(cases, "\n")}
                 |
                 |    r
                 |  }
                 |  val eventInPorts: MS[Z, Art.PortId] = MSZ(
                 |    ${(eventInPorts, ",\n")}
                 |  )
                 |  var frozen: MS[Art.PortId, Option[DataContent]] = MS()
                 |  var outgoing: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, None())
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
                 |          val (app, port) = connection(p)
                 |          Platform.send${if(isSharedMemory)"Async" else ""}(app, port, d)
                 |        case _ =>
                 |      }
                 |    }
                 |
                 |    for (p <- eventPortIds) {
                 |      outgoing(p) match {
                 |        case Some(d) =>
                 |          outgoing(p) = noData
                 |          val (app, port) = connection(p)
                 |          Platform.send${if(isSharedMemory)"Async" else ""}(app, port, d)
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
      val isSM: B = arsitOptions.ipc == Ipcmech.Shared_memory

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
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@ext object Process {
                 |  def sleep(n: Z): Unit = $$
                 |}
                 |"""
    }

    @pure def ProcessExt(packageName: String): ST = {
      return st"""package $packageName
                 |
                 |import org.sireum._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object Process_Ext {
                 |  def sleep(millis: Z): Unit = halt("stub")
                 |}"""
    }

    @pure def compile(arch: String): ST = {
      val mv = if(arch.toString == "win")
        "mv *.exe $SCRIPT_HOME/win/"
      else {
        s"""mv *_App $$SCRIPT_HOME/$arch/
           |${if(arsitOptions.ipc == Ipcmech.Message_queue) s"mv *_AEP $$SCRIPT_HOME/$arch/" else ""}
           |mv Main $$SCRIPT_HOME/$arch/""".stripMargin
      }
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |mkdir -p $arch
          |mkdir -p $$SCRIPT_HOME/../src/c/$arch
          |cd $$SCRIPT_HOME/../src/c/$arch
          |cmake -DCMAKE_BUILD_TYPE=Release ..
          |make $$MAKE_ARGS
          |$mv"""
    }

    @pure def run(aeps: ISZ[String],
                  apps: ISZ[String],
                  arch: String): ST = {
      val ext = if(arch.toString == "win") ".exe" else ""
      val staep = if(arsitOptions.ipc == Ipcmech.Message_queue) aeps.map(st => st"""$arch/$st$ext 2> /dev/null &""" ) else ISZ()
      val stapp = apps.map(st => {
        val prefix = arch.toString match {
          case "win" => "cygstart mintty /bin/bash"
          case "linux" => "x-terminal-emulator -e sh -c"
          case "mac" => "open -a Terminal"
        }
        st"""$prefix $arch/${st}$ext &""" })
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd $$SCRIPT_HOME
          |${(staep, "\n")}
          |${(stapp, "\n")}
          |read -p "Press enter to start ..."
          |$arch/Main$ext"""
    }

    @pure def stop(apps: ISZ[String]) : ST = {
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |APPS="${(apps, " ")}"
          |for APP in $${APPS}; do
          |  pkill $$APP
          |  pkill -9 $$APP
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
          |  ipcrm -q $$id;
          |done
          |
          |# semaphores
          |IPCS_Q=`ipcs -s | egrep "[0-9a-f]+[0-9]+" | grep $$ME | awk '{print $$2}'`
          |for id in $$IPCS_Q; do
          |  ipcrm -q $$id;
          |done
          |
          |ipcs
          |"""
    }

    @pure def transpiler(sourcepaths: ISZ[String],
                apps: ISZ[String],
                forwards: ISZ[String]): ST = {
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |export PROJ_HOME=$$SCRIPT_HOME/../src/main
          |export OUTPUT_DIR=$$SCRIPT_HOME/../src/c
          |export EXTS=$$OUTPUT_DIR/ext/ext.c:$$OUTPUT_DIR/ext/ipc.c
          |
          |if [ -n "$$1" ]; then
          |  TRANSPILER_JAR_HOME=$$1
          |fi
          |
          |if [ -z "$$TRANSPILER_JAR_HOME" ]; then
          |  echo "Specify the location of the Sireum-Transpiler jar file"
          |  exit 1
          |fi
          |
          |$$TRANSPILER_JAR_HOME transpiler c \
          |  --sourcepath $$PROJ_HOME \
          |  --apps "${(apps, ",")}" \
          |  --forward "${(forwards, ",")}" \
          |  --verbose \
          |  --bits 32 \
          |  --string-size 256 \
          |  --sequence-size 16 \
          |  --sequence ISZ[org.sireumString]=2 \
          |  --output-dir $$OUTPUT_DIR \
          |  --exts $$EXTS
          |"""
    }
  }
  // @formatter:on
}

object ArtNixGen{
  def apply(outputDir: File, m: Aadl, topPackage: String, nextPortId: Z, nextComponentId: Z, o: ArsitOption) : Z =
    new ArtNixGen().generator(outputDir, m, topPackage, nextPortId, nextComponentId, o)
}

