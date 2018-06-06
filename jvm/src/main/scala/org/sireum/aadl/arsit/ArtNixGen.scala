package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.util.MMap

class ArtNixGen {
  var nixOutputDir : File = _
  var cOutputDir : File = _
  var binOutputDir: File = _

  var topLevelPackageName: String = _

  val componentMap : MMap[String, Component] = org.sireum.util.mmapEmpty

  var id: Z = _
  def getPortId(): Z = {
    val r = id
    id = id + 1
    return r
  }

  def generator(nixOutputDir: File, cOutputDir: File, binOutputDir:File, m: Aadl, topPackageName: String, nextPortId: Z): Unit = {
    topLevelPackageName = Util.sanitizeName(topPackageName)
    this.nixOutputDir = nixOutputDir
    this.cOutputDir = cOutputDir
    this.binOutputDir = binOutputDir

    id = nextPortId

    gen(m)
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
    var inPorts: ISZ[String] = ISZ()
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

      val name: Names = Util.getNamesFromClassifier(m.classifier.get, topLevelPackageName)

      val bridgeInstanceVarName: String = Util.getName(m.identifier)
      val AEP_Id: String = s"${name.component}_AEP"
      val App_Id: String = s"${name.component}_App"

      val AEPPayloadTypeName: String = s"${AEP_Id}_Payload"

      var imports: ISZ[ST] = ISZ()
      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portOptResets: ISZ[ST] = ISZ()
      var aepPortCases: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      for (port <- m.features if Util.isInPort(port)) {
        val portName: String = Util.getLastName(port.identifier)
        val portIdName: String = portName + "PortId"
        val portOptName: String = portName + "Opt"
        val portType: String = Util.getPortType(port)
        val portPayloadTypeName: String = Util.getPortPayloadTypeName(port)
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${portName}"

        portDefs = portDefs :+ Template.portDef(portOptName, portType)
        portOpts = portOpts :+ Template.portOpt(portOptName, portType)
        portIds = portIds :+ Template.portId(portIdName, archPortInstanceName)
        aepPortCases = aepPortCases :+ Template.aepPortCase(portIdName, portOptName, portPayloadTypeName, isPeriodic)
        portOptResets = portOptResets :+ Template.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ Template.appCases(portOptName, portIdName, portPayloadTypeName)

        inPorts = inPorts :+ Util.getName(port.identifier)
      }

      platformPorts = platformPorts :+
        Template.platformPortDecl(AEP_Id, getPortId()) :+
        Template.platformPortDecl(App_Id, getPortId())

      if(portOpts.nonEmpty) {
        platformPayloads = platformPayloads :+
          Template.platformPayload(AEPPayloadTypeName, portDefs)
      }

      if(portOpts.nonEmpty)
        aepNames = aepNames :+ AEP_Id
      appNames = appNames :+ App_Id

      mainSends = mainSends :+
        Template.mainSend(AEP_Id) :+
        Template.mainSend(App_Id)

      val AEP_Payload = Template.AEPPayload(AEPPayloadTypeName, portOptNames)
      if(portOpts.nonEmpty) {
        val stAep = Template.aep(topLevelPackageName, imports, AEP_Id, portOpts,
          portIds, portOptResets, aepPortCases, AEP_Id, App_Id, AEP_Payload, isPeriodic)
        Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/${AEP_Id}.scala"), stAep)
      }

      val stApp = Template.app(topLevelPackageName, imports, App_Id, App_Id,
        AEP_Id, Util.getPeriod(m), bridgeInstanceVarName, AEP_Payload, portIds, appCases, isPeriodic)
      Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/${App_Id}.scala"), stApp)
    }

    var artNixCases: ISZ[ST] = ISZ()
    for(c <- connections) {
      val dstFullyQualifiedPortName = Util.getName(c.dst.feature)
      val dstComp = componentMap(Util.getName(c.dst.component))
      val dstArchPortInstanceName =
        s"${Util.getName(dstComp.identifier)}.${Util.getLastName(c.dst.feature)}"
      val name: Names = Util.getNamesFromClassifier(dstComp.classifier.get, topLevelPackageName)

      val srcComp = componentMap(Util.getName(c.src.component))
      val srcArchPortInstanceName =
          s"${Util.getName(srcComp.identifier)}.${Util.getLastName(c.src.feature)}"

      if(inPorts.elements.contains(dstFullyQualifiedPortName) &&
        Util.isThreadOrDevice(srcComp) && Util.isThreadOrDevice((dstComp))) {
        val dstCompAep = s"${name.component}_AEP"
        artNixCases = artNixCases :+
          Template.artNixCase(srcArchPortInstanceName, dstArchPortInstanceName, dstCompAep)
      }
    }

    platformPorts = platformPorts :+ Template.platformPortDecl("Main", getPortId())

    val stAep = Template.aep(topLevelPackageName, platformPorts, platformPayloads)
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/AEP.scala"), stAep)

    val stArtNix = Template.artNix(topLevelPackageName, artNixCases)
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/ArtNix.scala"), stArtNix)

    val stMain = Template.main(topLevelPackageName, mainSends)
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/Main.scala"), stMain)

    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/MessageQueue.scala"), Template.MessageQueue(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/MessageQueue_Ext.scala"), Template.MessageQueueExt(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/Platform.scala"), Template.platform(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/Platform_Ext.scala"), Template.PlatformExt(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/PlatformNix.scala"), Template.PlatformNix(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/Process.scala"), Template.Process(topLevelPackageName))
    Util.writeFile(new File(nixOutputDir, s"${topLevelPackageName}/Process_Ext.scala"), Template.ProcessExt(topLevelPackageName))


    Util.writeFile(new File(binOutputDir, "compile-cygwin.sh"), Template.compile("win"))
    Util.writeFile(new File(binOutputDir, "compile-linux.sh"), Template.compile("linux"))
    Util.writeFile(new File(binOutputDir, "compile-mac.sh"), Template.compile("mac"))

    Util.writeFile(new File(binOutputDir, "run-cygwin.sh"), Template.run(aepNames, appNames, "win"))
    Util.writeFile(new File(binOutputDir, "run-linux.sh"), Template.run(aepNames, appNames, "linux"))
    Util.writeFile(new File(binOutputDir, "run-mac.sh"), Template.run(aepNames, appNames, "mac"))

    Util.writeFile(new File(binOutputDir, "stop.sh"), Template.stop(aepNames, appNames))

    val extFile = new File(binOutputDir, "ext.c")
    Util.writeFile(extFile, st"""// add c extension code here""", false)

    val x = Template.transpiler(
      ISZ(new File(nixOutputDir, "../../../../art/src/main").getAbsolutePath, new File(nixOutputDir, "../").getAbsolutePath),
      ((aepNames ++ appNames) :+ "Main").map(s => s"${topLevelPackageName}.${s}"),
      ISZ(s"art.ArtNative=${topLevelPackageName}.ArtNix", s"${topLevelPackageName}.Platform=${topLevelPackageName}.PlatformNix"),
      cOutputDir.getAbsolutePath,
      extFile.getAbsolutePath
    )
    println(s"Use the following to transpile the project:\n${x.render}")
  }

  object Template {
    @pure def portDef(portOptName: String,
                      portType: String): ST =
      return st"""$portOptName: Option[$portType]"""

    @pure def portOpt(portOptName: String,
                      portType: String): ST =
      return st"""var ${portDef(portOptName, portType)} = None[$portType]()"""

    @pure def portId(portIdName: String,
                     archPortInstanceName: String): ST =
      return st"""val $portIdName = Arch.${archPortInstanceName}.id"""

    @pure def portOptReset(portOptName: String,
                          portType: String): ST =
      return st"""$portOptName = None[$portType]()"""

    @pure def aepPortCase(portIdName: String,
                          portOptName: String,
                          payloadTypeName: String,
                          isPeriodic: B): ST =
      return st"""case `$portIdName` =>
                 |  $portOptName = Some(d.asInstanceOf[$payloadTypeName].value)
                 |  ${if(!isPeriodic) "eventArrived()" else ""}"""

    @pure def AEPPayload(AEPPayloadTypeName: String,
                         portOptNames: ISZ[String]): ST =
      return st"""${AEPPayloadTypeName}(${(portOptNames, ", ")})"""

    @pure def appCases(portOptName: String,
                       portId: String,
                       payloadName: String): ST =
      return st"""${portOptName} match {
                 |  case Some(v) => ArtNix.updateData(${portId}, ${payloadName}(v))
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

    @pure def mainSend(portId: String): ST =
      return st"""Platform.send(IPCPorts.${portId}, IPCPorts.Main, empty)"""

    @pure def aep(packageName: String,
                  imports: ISZ[ST],
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
                 |${(imports, "\n")}
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
                  imports: ISZ[ST],
                  objectName: String,
                  IPCPort_Id: String,
                  AEP_Id: String,
                  period: ST,
                  bridge: String,
                  AEP_Payload: ST,
                  portIds: ISZ[ST],
                  cases: ISZ[ST],
                  isPeriodic: B
                 ) : ST = {
      return st"""// #Sireum

                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |${(imports, "\n")}
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
                 |    Platform.initialise(seed, Some(IPCPorts.${IPCPort_Id}))
                 |    ${if(portIds.nonEmpty) {
                          st"""
                              |val empty = art.Empty()
                              |val aepPortOpt: Option[Art.PortId] = Some(IPCPorts.${AEP_Id})""".render
                      } else ""}
                 |    val entryPoints = Arch.${bridge}.entryPoints
                 |
                 |    ${(portIds, "\n")}
                 |
                 |    Art.run(Arch.ad)
                 |
                 |    entryPoints.initialise()
                 |
                 |    Platform.receive(Some(IPCPorts.Main))
                 |
                 |    println("${objectName} starting ...")
                 |
                 |    while (true) {
                 |      ${if(isPeriodic) {
                            st"""Process.sleep(${period})
                                |ArtNix.timeDispatch()""".render
                        } else ""}
                 |      ${if(portIds.nonEmpty) {
                            st"""Platform.send(IPCPorts.${AEP_Id}, IPCPorts.${IPCPort_Id}, empty)
                                |val (_, d) = Platform.receive(aepPortOpt)
                                |${if(!isPeriodic) "ArtNix.eventDispatch()" else ""}
                                |val ${AEP_Payload} = d
                                |${(cases, "\n")}""".render
                         } else ""}
                 |      entryPoints.compute()
                 |    }
                 |    return 0
                 |  }
                 |
                 |  override def atExit(): Unit = {
                 |    Arch.${bridge}.entryPoints.finalise()
                 |    Platform.finalise()
                 |  }
                 |}"""
    }

    @pure def aep(packageName: String,
                  ports: ISZ[ST],
                  payloads: ISZ[ST]): ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |@enum object AEPState {
                 |  'Start
                 |  'EventArrived
                 |  'EventRequested
                 |}
                 |
                 |object IPCPorts {
                 |  ${(ports, "\n")}
                 |}
                 |
                 |${(payloads, "\n\n")}
                 |"""
    }
    @pure def artNix(packageName: String,
                     cases: ISZ[ST]): ST = {
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
                 |      for (i <- data.indices if data(i).nonEmpty) {
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
                 |    for (p <- eventPortIds) {
                 |      outgoing(p) match {
                 |        case Some(d) =>
                 |          val (app, port) = connection(p)
                 |          Platform.send(app, port, d)
                 |        case _ =>
                 |      }
                 |    }
                 |
                 |    for (p <- dataPortIds) {
                 |      outgoing(p) match {
                 |        case Some(d) =>
                 |          val (app, port) = connection(p)
                 |          Platform.send(app, port, d)
                 |        case _ =>
                 |      }
                 |    }
                 |
                 |    for (i <- outgoing.indices if outgoing(i).nonEmpty) {
                 |      outgoing(i) = noData
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
                 |  def create(msgid: Z): Z = ${"$"}
                 |  def get(msgid: Z): Z = ${"$"}
                 |  def send(msgid: Z, port: Art.PortId, d: DataContent): Unit = ${"$"}
                 |  def receive(): (Art.PortId, DataContent) = ${"$"}
                 |  def sendAsync(msgid: Z, port: Art.PortId, d: DataContent): B = ${"$"}
                 |  def receiveAsync(): Option[(Art.PortId, DataContent)] = ${"$"}
                 |  def remove(msgid: Z): Unit = ${"$"}
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
                 |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = ${"$"}
                 |  def receive(portOpt: Option[Art.PortId]): (Art.PortId, DataContent) = ${"$"}
                 |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = ${"$"}
                 |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = ${"$"}
                 |  def receiveAsync(portOpt: Option[Art.PortId]): Option[(Art.PortId, DataContent)] = ${"$"}
                 |  def finalise(): Unit = ${"$"}
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
                 |  var msgidOpt: Option[Z] = None()
                 |
                 |  def initialise(seed: Z, portOpt: Option[Art.PortId]): Unit = {
                 |    PlatformNix.seed = seed
                 |    portOpt match {
                 |      case Some(port) =>
                 |        val msgid = MessageQueue.create(seed + port)
                 |        msgidOpt = Some(msgid)
                 |      case _ =>
                 |    }
                 |  }
                 |
                 |  def receive(portOpt: Option[Art.PortId]): (Art.PortId, DataContent) = {
                 |    val p = MessageQueue.receive()
                 |    portOpt match {
                 |      case Some(port) => assert(p._1 == port)
                 |      case _ =>
                 |    }
                 |    return p
                 |  }
                 |
                 |  def send(app: Art.PortId, port: Art.PortId, data: DataContent): Unit = {
                 |    MessageQueue.send(seed + app, port, data)
                 |  }
                 |
                 |  def sendAsync(app: Art.PortId, port: Art.PortId, data: DataContent): B = {
                 |    val r = MessageQueue.sendAsync(seed + app, port, data)
                 |    return r
                 |  }
                 |
                 |  def receiveAsync(portOpt: Option[Art.PortId]): Option[(Art.PortId, DataContent)] = {
                 |    val pOpt = MessageQueue.receiveAsync()
                 |    (portOpt, pOpt) match {
                 |      case (Some(port), Some((p, _))) => assert(p == port); return pOpt
                 |      case _ => return None()
                 |    }
                 |  }
                 |
                 |  def finalise(): Unit = {
                 |    msgidOpt match {
                 |      case Some(msgid) => MessageQueue.remove(msgid)
                 |      case _ =>
                 |    }
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
                 |  def sleep(n: Z): Unit = ${"$"}
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
        s"""mv *_App ${"$"}SCRIPT_HOME/$arch/
           |mv *_AEP ${"$"}SCRIPT_HOME/$arch/
           |mv Main ${"$"}SCRIPT_HOME/$arch/""".stripMargin
      }
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=${"$"}( cd "${"$"}( dirname "${"$"}0" )" &> /dev/null && pwd )
          |cd ${"$"}SCRIPT_HOME
          |mkdir -p $arch
          |mkdir -p ${"$"}SCRIPT_HOME/../src/c/$arch
          |cd ${"$"}SCRIPT_HOME/../src/c/$arch
          |cmake -DCMAKE_BUILD_TYPE=Release ..
          |make ${"$"}MAKE_ARGS
          |$mv"""
    }

    @pure def run(aeps: ISZ[String],
                  apps: ISZ[String],
                  arch: String): ST = {
      val ext = if(arch.toString == "win") ".exe" else ""
      val staep = aeps.map(st => st"""$arch/$st$ext 2> /dev/null &""" )
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
          |export SCRIPT_HOME=${"$"}( cd "${"$"}( dirname "${"$"}0" )" &> /dev/null && pwd )
          |cd ${"$"}SCRIPT_HOME
          |${(staep, "\n")}
          |${(stapp, "\n")}
          |read -p "Press enter to start ..."
          |$arch/Main$ext"""
    }

    @pure def stop(aeps: ISZ[String],
                   apps: ISZ[String]) : ST = {
      val procs = aeps ++ apps.map(s => s + "_App")
      st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |APPS="${(procs, " ")}"
          |for APP in ${"$"}{APPS}; do
          |  pkill ${"$"}APP
          |  pkill -9 ${"$"}APP
          |done
          |ME=`whoami`
          |IPCS_Q=`ipcs -q | egrep "[0-9a-f]+[0-9]+" | grep ${"$"}ME | cut -f2 -d" "`
          |for id in ${"$"}IPCS_Q; do
          |  ipcrm -q ${"$"}id;
          |done
          |ipcs
          |"""
    }

    @pure def transpiler(sourcepaths: ISZ[String],
                         apps: ISZ[String],
                         forwards: ISZ[String],
                         outDir: String,
                         extFile: String): ST = {
      st"""java -jar ${"$"}transpiler_jar transpiler c --sourcepath ${(sourcepaths, ":")} --apps "${(apps, ",")}" --forward "${(forwards, ",")}" --verbose --bits 32 --string-size 256 --sequence-size 16 --sequence ISZ[org.sireumString]=2 --output-dir $outDir --exts $extFile"""
    }
  }
}

object ArtNixGen{
  def apply(nixOutputDir: File, cOutputDir: File, binOutputDir:File, m: Aadl, topPackage: String, nextPortId: Z):Unit =
    new ArtNixGen().generator(nixOutputDir, cOutputDir, binOutputDir, m, topPackage, nextPortId)
}

