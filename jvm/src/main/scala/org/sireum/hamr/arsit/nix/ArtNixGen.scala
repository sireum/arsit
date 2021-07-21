// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{SeL4NixTemplate, TranspilerTemplate}
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir.FeatureEnd
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.codegen.common.templates.TemplateUtil
import org.sireum.hamr.codegen.common.util.ResourceUtil

@record class ArtNixGen(val dirs: ProjectDirectories,
                        val root: AadlSystem,
                        val arsitOptions: ArsitOptions,
                        val symbolTable: SymbolTable,
                        val types: AadlTypes,
                        val previousPhase: Result
                       ) extends NixGen {

  val basePackage: String = arsitOptions.packageName

  var resources: ISZ[Resource] = ISZ()

  var transpilerOptions: ISZ[TranspilerConfig] = ISZ()

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
    assert(Util.isNix(arsitOptions.platform))

    gen(root)

    return ArsitResult(
      previousPhase.resources() ++ resources,
      portId,
      previousPhase.maxComponent,
      transpilerOptions)
  }

  def addExeResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createExeResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def addResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def gen(model: AadlSystem): Unit = {

    var platformPorts: ISZ[ST] = ISZ()
    var mainSends: ISZ[ST] = ISZ()
    var inPorts: ISZ[Port] = ISZ()
    var appNames: ISZ[String] = ISZ()
    var ext_h_entries: ISZ[ST] = ISZ()
    var ext_c_entries: ISZ[ST] = ISZ()

    val components = symbolTable.componentMap.values.filter(p =>
      p.isInstanceOf[AadlThread] || (p.isInstanceOf[AadlDevice] && arsitOptions.devicesAsThreads))
      .map((m: AadlComponent) => m.asInstanceOf[AadlThreadOrDevice])

    for (threadOrDevice <- components) {
      val component = threadOrDevice.component

      val names: Names = Names(component, basePackage)
      val ports: ISZ[Port] = Util.getPorts(threadOrDevice, types, basePackage, z"0")

      val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(component) match {
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
      val App_Id: String = s"${names.componentSingletonType}_App"

      var portDefs: ISZ[ST] = ISZ()
      var portOpts: ISZ[ST] = ISZ()
      var portIds: ISZ[ST] = ISZ()
      var portIdOpts: ISZ[ST] = ISZ()

      var portOptResets: ISZ[ST] = ISZ()
      var portOptNames: ISZ[String] = ISZ()
      var appCases: ISZ[ST] = ISZ()

      PropertyUtil.getStackSizeInBytes(component) match {
        case Some(bytes) => if (bytes > maxStackSize) {
          maxStackSize = bytes
        }
        case _ =>
      }

      val featureEnds = Util.getFeatureEnds(threadOrDevice.ports)
      val portSize = featureEnds.filter(f => CommonUtil.isInFeature(f.feature) || CommonUtil.isOutFeature(f.feature)).size
      if (portSize > maxPortsForComponents) {
        maxPortsForComponents = portSize
      }

      val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)

      for (p <- featureEnds if CommonUtil.isInFeature(p.feature)) {
        assert(CommonUtil.isPort(p.feature))

        val portName = p.identifier
        val isTrigger: B =
          if (dispatchTriggers.isEmpty) T
          else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty

        val port = Util.getPort(p, p.feature.asInstanceOf[FeatureEnd], component, types, basePackage, isTrigger, z"-1000")

        val portIdName: String = s"${port.name}PortId"
        val portOptName: String = s"${port.name}Opt"
        val portType: String = port.getPortTypeNames.qualifiedReferencedTypeName
        val archPortInstanceName: String = s"${bridgeInstanceVarName}.${port.name}"

        portDefs = portDefs :+ ArtNixTemplate.portDef(portOptName, portType)
        portOpts = portOpts :+ ArtNixTemplate.portOpt("", portOptName, portType, T)

        portIds = portIds :+ ArtNixTemplate.portId(portIdName, archPortInstanceName)
        portIdOpts = portIdOpts :+ ArtNixTemplate.portOpt(portIdName, portOptName, "Art.PortId", F)

        portOptResets = portOptResets :+ ArtNixTemplate.portOptReset(portOptName, portType)

        portOptNames = portOptNames :+ portOptName

        appCases = appCases :+ ArtNixTemplate.appCases(portOptName, portIdName, port.getPortTypeNames)

        inPorts = inPorts :+ port
      }

      platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl(App_Id, getPortId())
      mainSends = mainSends :+ ArtNixTemplate.mainSend(App_Id)
      appNames = appNames :+ App_Id

      val transpilerToucher = SeL4NixTemplate.transpilerToucher(basePackage)
      val transpilerToucherMethodCall = SeL4NixTemplate.callTranspilerToucher()

      addResource(
        dirs.componentDir,
        ISZ(basePackage, s"${SeL4NixTemplate.TRANSPILER_TOUCHER_OBJECT_NAME}.scala"),
        transpilerToucher,
        F) // don't overwrite since user may add contents to this file

      val apiTouches = SeL4NixTemplate.apiTouches(names, ports)
      val touchMethod = SeL4NixTemplate.genTouchMethod(genTypeTouches(types, basePackage), apiTouches)

      val stApp = ArtNixTemplate.app(
        packageName = basePackage,
        objectName = App_Id,
        IPCPort_Id = App_Id,
        period = CommonUtil.getPeriod(threadOrDevice),
        bridge = bridgeInstanceVarName,
        portIds = portIds,
        cases = appCases,
        component = threadOrDevice,
        isPeriodic = isPeriodic,
        types = types,
        touchMethod = touchMethod,
        basePackage = basePackage
      )

      addResource(dirs.slangNixDir, ISZ(basePackage, s"${App_Id}.scala"), stApp, T)

      val (_ext_h_entries, _ext_c_entries) = genExtensionEntries(component, names, ports)
      ext_h_entries = ext_h_entries ++ _ext_h_entries
      ext_c_entries = ext_c_entries ++ _ext_c_entries

      // don't care about paths since the root directory containing the 'ext-c'
      // dir will be passed to the transpiler rather than the individual resources
      val (paths, extResources) = genExtensionFiles(component, names, ports)

      resources = resources ++ extResources
    }

    {
      val extC = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_C
      val extH = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_H

      val uc = TemplateUtil.uniqueSTs(ext_c_entries)
      val uh = TemplateUtil.uniqueSTs(ext_h_entries)

      addResource(extC.up.value, ISZ(extC.name), SeL4NixTemplate.ext_c(uc), F)
      addResource(extH.up.value, ISZ(extH.name), SeL4NixTemplate.ext_h(uh), F)
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

        if (ops.ISZOps(inPorts.map((m: Port) => m.path)).contains(dstPath) &&
          (CommonUtil.isThread(srcComp) || CommonUtil.isDevice(srcComp)) && (CommonUtil.isThread(dstComp) || CommonUtil.isDevice(dstComp))) {
          val dstCompApp = s"${name.componentSingletonType}_App"
          var cases: ISZ[ST] = {
            if (artNixCasesM.contains(srcArchPortInstanceName)) {
              artNixCasesM.get(srcArchPortInstanceName).get
            } else {
              ISZ()
            }
          }
          cases = cases :+ ArtNixTemplate.artNixCase(dstCompApp, dstArchPortInstanceName)
          artNixCasesM = artNixCasesM + (srcArchPortInstanceName ~> cases)
        }
        numConnections = numConnections + 1
      } else {
        reporter.info(None(), Util.toolName, s"ArtNixGen: Skipping connection between ${srcComp.category} to ${dstComp.category}. ${CommonUtil.getName(c.name)}")
      }
    }

    platformPorts = platformPorts :+ ArtNixTemplate.platformPortDecl("Main", getPortId())

    arsitOptions.ipc match {
      case IpcMechanism.SharedMemory =>
        addResource(dirs.slangNixDir, ISZ(basePackage, "SharedMemory.scala"), ArtNixTemplate.SharedMemory(basePackage), T)
        addResource(dirs.slangNixDir, ISZ(basePackage, "SharedMemory_Ext.scala"), ArtNixTemplate.SharedMemory_Ext(basePackage), T)
      case x => halt(s"Unexpected IPC ${x}")
    }

    val stIPC = ArtNixTemplate.ipc(basePackage, platformPorts)
    addResource(dirs.slangNixDir, ISZ(basePackage, "IPC.scala"), stIPC, T)

    val artNixCases: ISZ[ST] = artNixCasesM.entries.map(k => ArtNixTemplate.artNixCases(k._1, k._2))
    val stArtNix = ArtNixTemplate.artNix(
      basePackage,
      artNixCases,
      inPorts.filter(p => CommonUtil.isEventPort(p.feature)).map(p => s"Arch.${p.parentPath}.${p.name}.id")
    )

    addResource(dirs.slangNixDir, ISZ(basePackage, "ArtNix.scala"), stArtNix, T)

    val stMain = ArtNixTemplate.main(basePackage, mainSends)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Main.scala"), stMain, T)

    addResource(dirs.slangNixDir, ISZ(basePackage, "Platform.scala"), ArtNixTemplate.platform(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Platform_Ext.scala"), ArtNixTemplate.PlatformExt(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "PlatformNix.scala"), ArtNixTemplate.PlatformNix(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Process.scala"), ArtNixTemplate.Process(basePackage), T)
    addResource(dirs.slangNixDir, ISZ(basePackage, "Process_Ext.scala"), ArtNixTemplate.ProcessExt(basePackage), T)

    resources = resources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(dirs.cBinDir.value, ISZ("compile.cmd")), ArtNixTemplate.compileSlash(dirs), T)

    for(plat <- ISZ(ArsitPlatform.Linux, ArsitPlatform.MacOS, ArsitPlatform.Cygwin)) {
      val platName = ops.StringOps(plat.name).firstToLower
      addExeResource(dirs.cBinDir.value, ISZ(s"run-${platName}.sh"), ArtNixTemplate.run(appNames, plat), T)
    }

    addExeResource(dirs.cBinDir.value, ISZ("stop.sh"), ArtNixTemplate.stop(appNames), T)

    addResource(dirs.cEtcDir, ISZ(NixGen.IPC_C), Util.getIpc(arsitOptions.ipc, basePackage), T)

    var outputPaths: ISZ[String] = ISZ(dirs.mainDir)
    if (!org.sireum.ops.StringOps(dirs.slangNixDir).contains(dirs.mainDir)) {
      outputPaths = outputPaths :+ dirs.slangNixDir
    }

    val excludes: ISZ[String] = if (arsitOptions.excludeImpl) {
      components.map((m: AadlThreadOrDevice) => {
        val name: Names = Names(m.component, basePackage)
        s"${name.packageName}.${name.componentSingletonType}"
      })
    } else {
      ISZ()
    }

    val buildApps: B = T

    val arraySizeInfluencers = ISZ(arsitOptions.maxArraySize, portId, previousPhase.maxComponent)

    val maxArraySize: Z = CommonUtil.findMaxZ(arraySizeInfluencers)

    val numPorts: Z = portId
    val numComponents: Z = previousPhase.maxComponent

    var customSequenceSizes: ISZ[String] = ISZ(
      s"IS[Z,art.Bridge]=${numComponents}",
      s"MS[Z,Option[art.Bridge]]=${numComponents}",
      s"IS[Z,art.UPort]=${maxPortsForComponents}",
      s"IS[Z,art.UConnection]=${numConnections}"

      // not valid
      //s"MS[org.sireum.Z,org.sireum.Option[art.UPort]]=${maxPortsForComponents}"
    )

    if (types.rawConnections) {
      val maxBitSize: Z = TypeUtil.getMaxBitsSize(symbolTable) match {
        case Some(z) => z
        case _ =>
          // model must only contain event ports (i.e. no data ports)
          1
      }
      customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${maxBitSize}"
    }

    val customConstants: ISZ[String] = ISZ(
      s"art.Art.maxComponents=${numComponents}",
      s"art.Art.maxPorts=${numPorts}"
    )

    val _extensions: ISZ[String] = ISZ(dirs.cExt_c_Dir, dirs.cEtcDir) ++ arsitOptions.auxCodeDirs

    val transpiler: (ST, TranspilerConfig) = TranspilerTemplate.transpiler(
      verbose = arsitOptions.verbose,
      libraryName = "main",
      sourcepaths = ISZ(dirs.mainDir),
      outputDir = Os.path(dirs.cNixDir),
      binDir = dirs.slangBinDir,
      apps = (appNames :+ "Main").map(s => s"${basePackage}.${s}"),
      forwards = ISZ(s"art.ArtNative=${basePackage}.ArtNix", s"${basePackage}.Platform=${basePackage}.PlatformNix"),
      numBits = arsitOptions.bitWidth,
      maxSequenceSize = maxArraySize,
      maxStringSize = arsitOptions.maxStringSize,
      customArraySizes = customSequenceSizes,
      customConstants = customConstants,
      stackSizeInBytes = maxStackSize,
      extensions = _extensions,
      excludes = excludes,
      buildApps = buildApps,
      cmakeIncludes = ISZ()
    )

    transpilerOptions = transpilerOptions :+ transpiler._2

    val slashTranspileScript = TranspilerTemplate.transpilerSlashScriptPreamble(ISZ(("main", transpiler._1)))
    resources = resources :+ ResourceUtil.createExeCrlfResource(Util.pathAppend(dirs.slangBinDir, ISZ("transpile.cmd")), slashTranspileScript, T)
  }
}