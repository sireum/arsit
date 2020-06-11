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

  var cOutputDir: String = _

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
    //val aepNames: ISZ[String] = ISZ()
    var appNames: ISZ[String] = ISZ()

    val components = symbolTable.airComponentMap.entries.filter(p =>
      CommonUtil.isThread(p._2) || (CommonUtil.isDevice(p._2) && arsitOptions.devicesAsThreads))

    var transpilerExtensions: ISZ[Os.Path] = getExistingCFiles(cExtensionDir)

    for ((archVarName, component) <- components) {

      val names: Names = Names(component, basePackage)
      val ports: ISZ[Port] = SlangUtil.getPorts(component, types, basePackage, z"0")

      val dispatchProtocol = PropertyUtil.getDispatchProtocol(component) match {
        case Some(x) => x
        case _ =>
          if (CommonUtil.isDevice(component)) {
            Dispatch_Protocol.Periodic
          } else {
            halt("HAMR codegen only supports Periodic or Sporadic threads")
          }
      }

      val isPeriodic: B = dispatchProtocol == Dispatch_Protocol.Periodic

      val bridgeInstanceVarName: String = CommonUtil.getName(component.identifier)
      val AEP_Id: String = s"${names.instanceName}_AEP"
      val App_Id: String = s"${names.instanceName}_App"

      //val AEPPayloadTypeName: String = s"${AEP_Id}_Payload"

      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portIdOpts: ISZ[ST] = ISZ()

      var portOptResets: ISZ[ST] = ISZ()
      //var aepPortCases: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      PropertyUtil.getStackSizeInBytes(component) match {
        case Some(bytes) => if (bytes > maxStackSize) {
          maxStackSize = bytes
        }
        case _ =>
      }

      val featureEnds = SlangUtil.getFeatureEnds(component.features)
      val portSize = featureEnds.filter(f => CommonUtil.isInFeature(f) || CommonUtil.isOutFeature(f)).size
      if (portSize > maxPortsForComponents) {
        maxPortsForComponents = portSize
      }

      val dispatchTriggers: Option[ISZ[String]] = SlangUtil.getDispatchTriggers(component)

      for (p <- featureEnds if CommonUtil.isInFeature(p)) {
        assert(CommonUtil.isPort(p))

        val portName = CommonUtil.getLastName(p.identifier)
        val isTrigger = if (dispatchTriggers.isEmpty) T else {
          dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
        }
        val port = SlangUtil.getPort(p, component, types, basePackage, isTrigger, z"-1000")

        val portIdName: String = s"${port.name}PortId"
        val portOptName: String = s"${port.name}Opt"
        val portType: String = port.getPortTypeNames.qualifiedReferencedTypeName
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${port.name}"

        portDefs = portDefs :+ ArtNixTemplate.portDef(portOptName, portType)
        portOpts = portOpts :+ ArtNixTemplate.portOpt("", portOptName, portType, T)

        portIds :+= ArtNixTemplate.portId(portIdName, archPortInstanceName)
        portIdOpts :+= ArtNixTemplate.portOpt(portIdName, portOptName, "Art.PortId", F)

        //aepPortCases = aepPortCases :+ ArtNixTemplate.aepPortCase(portIdName, portOptName, port.getPortTypeNames, CommonUtil.isAadlDataPort(p))

        portOptResets = portOptResets :+ ArtNixTemplate.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ ArtNixTemplate.appCases(portOptName, portIdName, port.getPortTypeNames)

        inPorts :+= port
      }

      platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl(App_Id, getPortId())
      mainSends :+= ArtNixTemplate.mainSend(App_Id)
      appNames = appNames :+ App_Id

      val stApp = ArtNixTemplate.app(
        packageName = basePackage,
        objectName = App_Id,
        IPCPort_Id = App_Id,
        AEP_Id = AEP_Id,
        period = Util.getPeriod(component),
        bridge = bridgeInstanceVarName,
        portIds = portIds,
        cases = appCases,
        component = component,
        isPeriodic = isPeriodic,
        types = types,
        basePackage = basePackage
      )

      addResource(dirs.nixDir, ISZ(basePackage, s"${App_Id}.scala"), stApp, T)

      val (paths, extResources) = genExtensionFiles(component, names, ports)
      resources = resources ++ extResources
      transpilerExtensions = transpilerExtensions ++ paths
    }

    var artNixCasesM: HashSMap[String, ISZ[ST]] = HashSMap.empty
    for (c <- symbolTable.connections) {
      val dstComp = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get
      val srcComp = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get

      if ((CommonUtil.isDevice(srcComp) || CommonUtil.isThread(srcComp)) &&
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
          cases = cases :+ ArtNixTemplate.artNixCase(dstComp, dstArchPortInstanceName)
          artNixCasesM = artNixCasesM + (srcArchPortInstanceName, cases)
        }
        numConnections = numConnections + 1
      } else {
        reporter.info(None(), Util.toolName, s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${CommonUtil.getName(c.name)}")
      }
    }

    platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case Cli.IpcMechanism.SharedMemory =>
        addResource(dirs.nixDir, ISZ(basePackage, "SharedMemory.scala"), ArtNixTemplate.SharedMemory(basePackage), T)
        addResource(dirs.nixDir, ISZ(basePackage, "SharedMemory_Ext.scala"), ArtNixTemplate.SharedMemory_Ext(basePackage), T)
      case x => halt(s"Unexpected IPC ${x}")
    }

    val stIPC = ArtNixTemplate.ipc(basePackage, platformPorts)
    addResource(dirs.nixDir, ISZ(basePackage, "IPC.scala"), stIPC, T)

    val artNixCases: ISZ[ST] = artNixCasesM.entries.map(k => ArtNixTemplate.artNixCases(k._1, k._2))
    val stArtNix = ArtNixTemplate.artNix(
      basePackage,
      artNixCases,
      inPorts.filter(p => CommonUtil.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id")
    )

    addResource(dirs.nixDir, ISZ(basePackage, "ArtNix.scala"), stArtNix, T)

    val stMain = ArtNixTemplate.main(basePackage, mainSends)
    addResource(dirs.nixDir, ISZ(basePackage, "Main.scala"), stMain, T)

    addResource(dirs.nixDir, ISZ(basePackage, "Platform.scala"), ArtNixTemplate.platform(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Platform_Ext.scala"), ArtNixTemplate.PlatformExt(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "PlatformNix.scala"), ArtNixTemplate.PlatformNix(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Process.scala"), ArtNixTemplate.Process(basePackage), T)
    addResource(dirs.nixDir, ISZ(basePackage, "Process_Ext.scala"), ArtNixTemplate.ProcessExt(basePackage), T)

    val platName = ops.StringOps(arsitOptions.platform.name).firstToLower

    addExeResource(dirs.binDir, ISZ(s"compile-${platName}.sh"), ArtNixTemplate.compile(arsitOptions.platform, dirs, cOutputDir), T)
    addExeResource(dirs.binDir, ISZ(s"run-${platName}.sh"), ArtNixTemplate.run(appNames, arsitOptions.platform), T)
    addExeResource(dirs.binDir, ISZ("stop.sh"), ArtNixTemplate.stop(appNames), T)

    addResource(cExtensionDir, ISZ("ipc.c"), Util.getIpc(arsitOptions.ipc, basePackage), T)

    var outputPaths: ISZ[String] = ISZ(dirs.srcMainDir)
    if (!org.sireum.ops.StringOps(dirs.nixDir).contains(dirs.srcMainDir))
      outputPaths = outputPaths :+ dirs.nixDir

    val excludes: ISZ[String] = if (arsitOptions.excludeImpl) {
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

    if (types.rawConnections) {
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

    val transpiler = ArtNixTemplate.transpiler(
      (appNames :+ "Main").map(s => s"${basePackage}.${s}"),
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
      additionalInstructions,
      dirs,
      cOutputDir
    )

    transpilerOptions = transpilerOptions :+ transpiler._2
    addExeResource(dirs.binDir, ISZ(transpileScriptName), transpiler._1, T)
  }
}