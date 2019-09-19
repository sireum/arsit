package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._

class ArtNixGen(outputDir: File,
                m: Aadl,
                topPackageName: String,
                nextPortId: Z,
                nextComponentId: Z,
                arsitOptions: Cli.ArsitOption,
                types: AadlTypes) {
  var projOutputDir: File = _
  var nixOutputDir : File = _

  var cOutputDir : File = _
  var cOutputDirString : String = _

  var cExtensionDir : File = _
  var cUserExtensionDir : Option[File] = None()

  var binOutputDir: File = _

  var basePackage: String = _

  var componentMap : HashMap[String, Component] = HashMap.empty

  var systemImplmentation: Component = _

  var portId: Z = _
  def getPortId(): Z = {
    val r = portId
    portId = portId + 1
    return r
  }

  def generator(): Z = {
    basePackage = Util.sanitizeName(topPackageName)
    projOutputDir = outputDir // where the slang-embedded code was generated

    binOutputDir = new File(projOutputDir, "../../bin")

    cOutputDir = if(arsitOptions.outputCDir.nonEmpty) {
      new File(arsitOptions.outputCDir.get.native)
    } else {
      val dir = arsitOptions.platform match {
        case Cli.Platform.SeL4 => "sel4"
        case o =>
          assert(SlangUtil.isNix(o))
          "nix"
      }
      new File(projOutputDir, s"../c/${dir}")
    }

    cExtensionDir = if(arsitOptions.behaviorDir.nonEmpty) {
      new File(arsitOptions.behaviorDir.get.native)
    } else {
      new File(projOutputDir, s"../c/ext-c")
    }

    nixOutputDir = new File(projOutputDir, "nix")

    if(arsitOptions.behaviorDir.nonEmpty) {
      cUserExtensionDir = Some(new File(arsitOptions.behaviorDir.get.native))
    }

    portId = nextPortId

    gen(m)

    portId
  }

  def gen(model: Aadl): Unit = {

    var connections: ISZ[ConnectionInstance] = ISZ()

    assert(model.components.size == 1)
    assert(Util.isSystem(model.components(0)))
    systemImplmentation = model.components(0)

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

    val components = componentMap.entries.filter(p =>
      Util.isThread(p._2) || (Util.isDevice(p._2) && arsitOptions.devicesAsThreads))

    for ((archVarName, m) <- components) {

      val name: Names = Util.getNamesFromClassifier(m.classifier.get, basePackage)

      val dispatchProtocol = Util.getSlangEmbeddedDispatchProtocol(m)

      val isPeriodic: B = dispatchProtocol == DispatchProtocol.Periodic

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
        val _portType = Util.getFeatureEndType(p, types)
        val port = Port(p, m, _portType, basePackage)
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
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/${AEP_Id}.scala"), stAep)
      }

      val stApp = Template.app(basePackage, App_Id, App_Id,
        AEP_Id, Util.getPeriod(m), bridgeInstanceVarName, AEP_Payload, portIds, appCases, m, isPeriodic)
      Util.writeFile(new File(nixOutputDir, s"${basePackage}/${App_Id}.scala"), stApp)
    }

    var artNixCasesM: HashSMap[String, ISZ[ST]] = HashSMap.empty
    for(c <- connections) {
      val dstComp = componentMap.get(Util.getName(c.dst.component)).get
      val srcComp = componentMap.get(Util.getName(c.src.component)).get

      if((Util.isDevice(srcComp) || Util.isThread(srcComp)) & (Util.isDevice(dstComp) || Util.isThread(dstComp))) {
        val dstPath = Util.getName(c.dst.feature.get)
        val dstArchPortInstanceName = s"${Util.getName(dstComp.identifier)}.${Util.getLastName(c.dst.feature.get)}"
        val name: Names = Util.getNamesFromClassifier(dstComp.classifier.get, basePackage)

        val srcArchPortInstanceName =
          s"${Util.getName(srcComp.identifier)}.${Util.getLastName(c.src.feature.get)}"

        if (inPorts.map(_.path).elements.contains(dstPath) &&
          (Util.isThread(srcComp) || Util.isDevice(srcComp)) && (Util.isThread(dstComp) || Util.isDevice(dstComp))) {
          val dstComp = if (arsitOptions.ipc == Cli.IpcMechanism.SharedMemory) s"${name.component}_App" else s"${name.component}_AEP"
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
      } else {
        Util.report(s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${Util.getName(c.name)}", T)
      }
    }

    platformPorts = platformPorts :+ Template.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case Cli.IpcMechanism.MessageQueue =>

        Util.writeFile(new File(nixOutputDir, s"${basePackage}/MessageQueue.scala"), Template.MessageQueue(basePackage))
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/MessageQueue_Ext.scala"), Template.MessageQueueExt(basePackage))

      case Cli.IpcMechanism.SharedMemory =>
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/SharedMemory.scala"), Template.SharedMemory(basePackage))
        Util.writeFile(new File(nixOutputDir, s"${basePackage}/SharedMemory_Ext.scala"), Template.SharedMemory_Ext(basePackage))
    }

    val stIPC = Template.ipc(basePackage, platformPorts, platformPayloads)
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/IPC.scala"), stIPC)

    val artNixCases: ISZ[ST] = artNixCasesM.entries.map(k => Template.artNixCases(k._1, k._2))
    val stArtNix = Template.artNix(basePackage, artNixCases,
      inPorts.filter(p => Util.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id"))

    Util.writeFile(new File(nixOutputDir, s"${basePackage}/ArtNix.scala"), stArtNix)

    val stMain = Template.main(basePackage, mainSends)
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Main.scala"), stMain)

    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Platform.scala"), Template.platform(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Platform_Ext.scala"), Template.PlatformExt(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/PlatformNix.scala"), Template.PlatformNix(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Process.scala"), Template.Process(basePackage))
    Util.writeFile(new File(nixOutputDir, s"${basePackage}/Process_Ext.scala"), Template.ProcessExt(basePackage))

    arsitOptions.platform match {

      case o @ Cli.Platform.SeL4 =>

        // just compile static lib.  place script in parent dir so transpiler won't
        // erase it

        val f = new File(cOutputDir, s"../compile-hamr-lib.sh")
        Util.writeFile(f, Template.compileLib(o, cOutputDir.getName))

        import sys.process._
        Seq("/bin/chmod", "-R", "u+x", f.getAbsolutePath).!!

      case o =>
        assert(SlangUtil.isNix(o))
        val platName = ops.StringOps(o.name).firstToLower

        Util.writeFile(new File(binOutputDir, s"compile-${platName}.sh"), Template.compile(o))
        Util.writeFile(new File(binOutputDir, s"run-${platName}.sh"), Template.run(aepNames, appNames, o))
        Util.writeFile(new File(binOutputDir, "stop.sh"), Template.stop(
          (if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aepNames else ISZ[String]()) ++ appNames))
    }

    Util.writeFile(new File(cExtensionDir, s"ipc.c"), Util.getIpc(arsitOptions.ipc, basePackage))
    Util.writeFile(new File(cExtensionDir, s"ext.c"), Util.getLibraryFile("ext.c"), false)
    Util.writeFile(new File(cExtensionDir, s"ext.h"), Util.getLibraryFile("ext.h"), false)

    var outputPaths: ISZ[String] = ISZ(projOutputDir.getAbsolutePath)
    if(!nixOutputDir.getAbsolutePath.contains(projOutputDir.getAbsolutePath))
      outputPaths = outputPaths :+ nixOutputDir.getAbsolutePath

    val excludes:ISZ[String] = if(arsitOptions.excludeImpl) {
      for ((archVarName, m) <- components) yield {
        val name: Names = Util.getNamesFromClassifier(m.classifier.get, basePackage)
        s"${name.packageName}.${name.componentImpl}"
      }
    } else {
      ISZ()
    }

    var transpileScriptName = ""
    var extensions: ISZ[String] = ISZ(s"${cExtensionDir.getCanonicalPath}/ext.c", s"${cExtensionDir.getCanonicalPath}/ext.h")
    var buildApps: B = T
    var additionalInstructions: Option[ST] = None()

    arsitOptions.platform match {
      case Cli.Platform.SeL4 =>
        transpileScriptName = "transpile-sel4.sh"
        buildApps = F
      case o =>
        assert(SlangUtil.isNix(o))
        extensions = extensions :+ s"${cExtensionDir.getCanonicalPath}/ipc.c"
        transpileScriptName = s"transpile.sh"
    }

    val transpiler = Template.transpiler(
      outputPaths,
      (((if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aepNames else ISZ[String]()) ++ appNames) :+ "Main").map(s => s"${basePackage}.${s}"),
      ISZ(s"art.ArtNative=${basePackage}.ArtNix", s"${basePackage}.Platform=${basePackage}.PlatformNix"),
      arsitOptions.bitWidth, //  Util.getDefaultBitWidth(systemImplmentation),
      arsitOptions.maxArraySize, // Util.getDefaultMaxSequenceSize(systemImplmentation, portId),
      arsitOptions.maxStringSize, // Util.getMaxStringSize(systemImplmentation),
      extensions,
      excludes,
      buildApps,
      additionalInstructions
    )

    Util.writeFile(new File(binOutputDir, transpileScriptName), transpiler)

    import sys.process._
    Seq("/bin/chmod", "-R", "u+x", binOutputDir.getAbsolutePath).!!
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
      val isSharedMemory = arsitOptions.ipc == Cli.IpcMechanism.SharedMemory
      def portId(p: Port) = s"${p.name}PortId"
      def portIdOpt(p: Port) = s"${portId(p)}Opt"
      val inPorts = Util.getFeatureEnds(component.features).filter(p => Util.isPort(p) && Util.isInFeature(p)).map(f => {
        val pType = Util.getFeatureEndType(f, types)
        Port(f, component, pType, basePackage)
      })

      var globals: ISZ[ST] = ISZ(st"""val entryPoints: Bridge.EntryPoints = Arch.${bridge}.entryPoints
                                     |val appPortId: Art.PortId = IPCPorts.${IPCPort_Id}
                                     |val appPortIdOpt: Option[Art.PortId] = Some(appPortId)""")

      var inits: ISZ[ST] = ISZ(st"Platform.initialise(seed, appPortIdOpt)")

      if(!isSharedMemory && portIds.nonEmpty) {
        globals = globals :+ st"""val empty = art.Empty()
                                 |val aepPortId = IPCPorts.${AEP_Id}
                                 |val aepPortIdOpt: Option[Art.PortId] = Some(aepPortId)"""
      }

      val portSection =
        if(isSharedMemory) {
          for(p <- inPorts) yield {
            globals = globals :+
              st"val ${portId(p)}: Art.PortId = Arch.${bridge}.${p.name}.id" :+
              st"val ${portIdOpt(p)}: Option[Art.PortId] = Some(${portId(p)})"

            inits = inits :+ st"Platform.initialise(seed, ${portIdOpt(p)})"
          }
        } else
          portIds



      var computeBody: ST = st""

      // @formatter:off
      val body = {
        if(isSharedMemory) {
          /********** SHARED MEMORY BODY **********/

          val loopBody = {

            val receiveOnInPorts =
              for(p <- inPorts) yield {
                val dispatch = if(Util.isDataPort(p.feature)) { "F" } else { "T" }
                st"""Platform.receiveAsync(${portIdOpt(p)}) match {
                    |  case Some((_, v: ${p.portType.qualifiedPayloadName})) => ArtNix.updateData(${portId(p)}, v)${ if(!isPeriodic) s"; dispatch = ${dispatch}" else "" }
                    |  case Some((_, v)) => halt(s"Unexpected payload on port $${${portId(p)}}.  Expecting something of type ${p.portType.qualifiedPayloadName} but received $${v}")
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
                 |  ${(globals, "\n")}
                 |
                 |  def initialise(seed: Z): Unit = {
                 |    ${(inits, "\n")}
                 |
                 |    Art.run(Arch.ad)
                 |  }
                 |
                 |  def compute(): Unit = {
                 |    ${computeBody}
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
                 |    initialise(seed)
                 |
                 |    Platform.receive(${if(isSharedMemory) "appPortIdOpt" else "Some(IPCPorts.Main)"}) // pause after setting up component
                 |
                 |    entryPoints.initialise()
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
                 |  val data: MS[Art.PortId, Option[DataContent]] = MS.create(maxPortIds, None())
                 |  val noData: Option[DataContent] = None()
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

    @pure def compileLib(arch: Cli.Platform.Type, childDir: String): ST = {
      val script_home = "${SCRIPT_HOME}"

      return st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |export SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |cd "${script_home}/${childDir}"
          |mkdir -p sel4-build
          |cd sel4-build
          |cmake ..
          |make $$MAKE_ARGS
          |"""
    }

    @pure def compile(arch: Cli.Platform.Type): ST = {
      assert(SlangUtil.isNix(arch))

      val script_home = "${SCRIPT_HOME}"
      val pathSep = '/'
      val binCan = binOutputDir.getCanonicalPath
      val cOutputDirRel = SlangUtil.relativizePaths(binCan, cOutputDir.getCanonicalPath, pathSep, script_home)

      val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
      val mv: ST = if(arch == Cli.Platform.Cygwin)
        st"mv *.exe ${script_home}/${buildDir}/"
      else {
        st"""mv *_App ${script_home}/${buildDir}/
            |${if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) s"mv *_AEP ${script_home}/${buildDir}/" else ""}
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
                  arch: Cli.Platform.Type): ST = {
      assert(SlangUtil.isNix(arch))

      val buildDir = st"${ops.StringOps(arch.name).firstToLower}-build"
      val ext = if(arch.toString == "win") ".exe" else ""
      val staep = if(arsitOptions.ipc == Cli.IpcMechanism.MessageQueue) aeps.map(st => st"""$arch/$st$ext 2> /dev/null &""" ) else ISZ()
      val stapp = apps.map(st => {
        val prefix = arch match {
          case Cli.Platform.Cygwin => "cygstart mintty /bin/bash"
          case Cli.Platform.Linux => "x-terminal-emulator -e sh -c"
          case Cli.Platform.MacOS => "open -a Terminal"
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
                         extensions: ISZ[String],
                         excludes: ISZ[String],
                         buildApps: B,
                         additionalInstructions: Option[ST]): ST = {

      val script_home = "${SCRIPT_HOME}"
      val pathSep = '/'
      val binCan = binOutputDir.getCanonicalPath
      val cOutputDirRel = SlangUtil.relativizePaths(binCan, cOutputDir.getCanonicalPath, pathSep, script_home)
      val extsRel = extensions.map(s => SlangUtil.relativizePaths(binCan, s, pathSep, script_home))

      var ret = st"""#!/usr/bin/env bash
          |#
          |# This file is autogenerated.  Do not edit
          |#
          |set -e
          |SCRIPT_HOME=$$( cd "$$( dirname "$$0" )" &> /dev/null && pwd )
          |PROJ_HOME="${script_home}/../src/main"
          |OUTPUT_DIR="${cOutputDirRel}"
          |
          |if [ -z "$${SIREUM_HOME}" ]; then
          |  echo "SIREUM_HOME not set"
          |  exit 1
          |fi
          |
          |$${SIREUM_HOME}/bin/sireum slang transpilers c \
          |  --sourcepath "$${PROJ_HOME}" \
          |  --apps "${(apps, ",")}" \
          |  --forward "${(forwards, ",")}" \
          |  --bits ${numBits} \
          |  --string-size ${maxStringSize} \
          |  --sequence-size ${maxSequenceSize} \
          |  --output-dir "$${OUTPUT_DIR}" \
          |  --stable-type-id"""

      ret = if(extsRel.nonEmpty) {
        st"""${ret} \
            |  --exts "${(extsRel, ":")}""""
      } else { ret }

      ret = if(excludes.nonEmpty) {
        st"""${ret} \
            |  --exclude-build "${(excludes, ",")}""""
      } else { ret }

      ret = if(!buildApps) {
        st"""${ret} \
            |  --lib-only"""
      } else { ret }

      ret = if(additionalInstructions.nonEmpty) {
        st"""${ret}
            |
            |${additionalInstructions}"""
      } else { ret }

      return st"""
              |$ret"""
    }
  }
  // @formatter:on
}

object ArtNixGen{
  def apply(outputDir: File, m: Aadl, topPackage: String, nextPortId: Z, nextComponentId: Z, o: Cli.ArsitOption, types: AadlTypes) : Z =
    new ArtNixGen(outputDir, m, topPackage, nextPortId, nextComponentId, o, types).generator()
}

