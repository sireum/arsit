// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{ArchitectureTemplate, CMakeTemplate, SeL4NixTemplate, TranspilerTemplate}
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform}
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names, StringUtil}

@record class SeL4NixGen(val dirs: ProjectDirectories,
                         val root: AadlSystem,
                         val arsitOptions: ArsitOptions,
                         val symbolTable: SymbolTable,
                         val types: AadlTypes,
                         val previousPhase: Result
                        ) extends NixGen {

  val basePackage: String = arsitOptions.packageName

  var resources: ISZ[Resource] = ISZ()

  var transpilerOptions: ISZ[TranspilerConfig] = ISZ()

  val defaultMaxStackSizeInBytes: Z = z"16" * z"1024" * z"1024"

  val useArm: B = ops.ISZOps(symbolTable.getProcesses()).exists(p => p.toVirtualMachine())

  def generate(): ArsitResult = {
    assert(arsitOptions.platform == ArsitPlatform.SeL4)

    gen(root)

    return ArsitResult(
      previousPhase.resources() ++ resources,
      previousPhase.maxPort,
      previousPhase.maxComponent,
      transpilerOptions)
  }

  def addExeResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createExeResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def addResource(outDir: String, path: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(outDir, path), content, overwrite)
  }

  def gen(root: AadlSystem): Unit = {

    val extC = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_C
    val extH = Os.path(dirs.cExt_c_Dir) / NixGen.EXT_H

    var ext_h_entries: ISZ[ST] = ISZ()
    var ext_c_entries: ISZ[ST] = ISZ()

    val components: ISZ[AadlThreadOrDevice] = symbolTable.componentMap.values.filter(p =>
      p.isInstanceOf[AadlThread] || (p.isInstanceOf[AadlDevice] && arsitOptions.devicesAsThreads))
      .map(m => m.asInstanceOf[AadlThreadOrDevice])

    var transpilerScripts: Map[String, (ST, TranspilerConfig)] = Map.empty

    val typeTouches = NixGen.genTypeTouches(types, basePackage)

    for (component <- components) {

      val names: Names = Names(component.component, basePackage)

      val ports: ISZ[Port] = Util.getPorts(component, types, basePackage, z"0")

      val instanceSingletonName: String = names.componentSingletonType

      val globals: ST = genGlobals(ports, names)

      val receiveInput: ST = genReceiveInput(ports, names)

      val putValue: ST = genPutValue(ports, names.identifier)

      val getValue: ST = genGetValue(ports, names.identifier)

      val sendOutput: ST = genSendOutput(ports, names)

      val period: Z = CommonUtil.getPeriod(component)

      val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(component.component).get
      val dispatchStatus = genDispatchStatus(names, ports, dispatchProtocol)

      val _ports = ports.map((p: Port) => ArchitectureTemplate.genPort(p))
      val _portArgs = ports.map((p: Port) => st"${p.name} = ${p.name}")

      val bridge = ArchitectureTemplate.bridge(
        names.bridgeIdentifier,
        names.instanceName,
        names.bridgeTypeName,
        z"0",
        ArchitectureTemplate.dispatchProtocol(dispatchProtocol, period),
        Util.getDispatchTriggers(component.component),
        _ports,
        _portArgs)

      val transpilerToucher = SeL4NixTemplate.transpilerToucher(basePackage)
      val transpilerToucherMethodCall = SeL4NixTemplate.callTranspilerToucher()

      addResource(
        dirs.componentDir,
        ISZ(basePackage, s"${SeL4NixTemplate.TRANSPILER_TOUCHER_OBJECT_NAME}.scala"),
        transpilerToucher,
        F) // DON'T overwrite as user's will add contents to this file

      val apiTouches = SeL4NixTemplate.apiTouches(names, ports)
      val touchMethod = SeL4NixTemplate.genTouchMethod(typeTouches, apiTouches, ISZ())

      val app = SeL4NixTemplate.app(
        basePackage,
        instanceSingletonName,
        ISZ(s"import ${names.basePackage}._", s"import ${names.packageName}.${names.sel4SlangExtensionName}"),
        names.identifier,
        bridge,
        names.bridgeIdentifier,
        dispatchStatus,
        globals,
        receiveInput,
        getValue,
        putValue,
        sendOutput,
        touchMethod,
        transpilerToucherMethodCall)

      addResource(
        dirs.seL4NixDir,
        ISZ(basePackage, instanceSingletonName, s"${names.identifier}.scala"),
        app,
        T)

      { // extension objects

        val slangExtensionObject: ST = genSlangSel4ExtensionObject(ports, names)

        addResource(
          dirs.seL4NixDir,
          ISZ(names.packagePath, s"${names.sel4SlangExtensionName}.scala"),
          slangExtensionObject,
          T)

        val slangExtensionObjectStub: ST = genSlangSel4ExtensionObjectStub(ports, basePackage, names)

        addResource(
          dirs.seL4NixDir,
          ISZ(names.packagePath, s"${names.sel4SlangExtensionStubName}.scala"),
          slangExtensionObjectStub,
          T)
      }

      val cOutputDir: Os.Path = dirs.cOutputPlatformDir / instanceSingletonName

      val (_ext_h_entries, _ext_c_entries) = genExtensionEntries(component.component, names, ports)
      ext_h_entries = ext_h_entries ++ _ext_h_entries
      ext_c_entries = ext_c_entries ++ _ext_c_entries

      val (paths, extResources) = genExtensionFiles(component.component, names, ports)
      resources = resources ++ extResources

      val transpilerExtensions: ISZ[Os.Path] = (extC +: (extH +: paths)) ++ genSel4Adapters(names)

      val stackSizeInBytes: Z = PropertyUtil.getStackSizeInBytes(component.component) match {
        case Some(size) => size
        case _ => defaultMaxStackSizeInBytes
      }

      val settingsFilename = s"${dirs.slangBinDir}/${CMakeTemplate.cmake_settingsFilename(instanceSingletonName)}"
      addResource(settingsFilename, ISZ(), CMakeTemplate.cmake_sel4_settings_cmake(instanceSingletonName), F)

      // prefix with '+' to indicate settings should come after library definitions
      val plusSettingsFilename = s"+${settingsFilename}"

      val trans = genTranspiler(
        basePackage = basePackage,
        names = names,
        maxStackSizeInBytes = stackSizeInBytes,
        numComponentInPorts = ports.filter(p => CommonUtil.isInPort(p.feature)).size,
        numComponentOutPorts = ports.filter(p => CommonUtil.isOutPort(p.feature)).size,
        cOutputDir = cOutputDir,
        cExtensions = transpilerExtensions,
        cmakeIncludes = ISZ(plusSettingsFilename)
      )

      transpilerScripts = transpilerScripts + (instanceSingletonName ~> trans)
    }

    {
      val _ext_c_entries: ISZ[ST] = (Set.empty[String] ++ ext_c_entries.map((s: ST) => s.render)).elements.map((s: String) => st"${s}")
      val _ext_h_entries: ISZ[ST] = (Set.empty[String] ++ ext_h_entries.map((s: ST) => s.render)).elements.map((s: String) => st"${s}")

      addResource(extC.up.value, ISZ(extC.name), SeL4NixTemplate.ext_c(_ext_c_entries), F)
      addResource(extH.up.value, ISZ(extH.name), SeL4NixTemplate.ext_h(_ext_h_entries), F)
    }

    { // Slang Type Library

      val id = "SlangTypeLibrary"
      val cOutputDir: Os.Path = dirs.cOutputPlatformDir / id
      val relPath = s"${cOutputDir.up.name}/${id}"


      val typeApp = SeL4NixTemplate.typeApp(basePackage, id, id, typeTouches)

      addResource(dirs.seL4NixDir,
        ISZ(basePackage, id, s"${id}.scala"),
        typeApp,
        T
      )

      var customSequenceSizes: ISZ[String] = ISZ()
      if (types.rawConnections) {
        // TODO is this necessary?
        val maxBitSize: Z = TypeUtil.getMaxBitsSize(symbolTable) match {
          case Some(z) => z
          case _ =>
            // model must only contain event ports (i.e. no data ports)
            1
        }
        customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${maxBitSize}"
      }


      val settingsFilename = s"${dirs.slangBinDir}/${CMakeTemplate.cmake_settingsFilename(id)}"
      addResource(settingsFilename, ISZ(), CMakeTemplate.cmake_sel4_settings_cmake(id), F)

      // prefix with '+' to indicate settings should come after library definitions
      val plusSettingsFilename = s"+${settingsFilename}"

      val trans = genTranspilerBase(
        basePackage = basePackage,
        instanceName = id,
        identifier = id,
        sourcePaths = ISZ(),
        cOutputDir = cOutputDir,

        maxSequenceSize = 1,

        customSequenceSizes = customSequenceSizes,
        customConstants = ISZ(),
        maxStackSizeInBytes = defaultMaxStackSizeInBytes,

        extensions = ISZ(),
        excludes = ISZ(),

        cmakeIncludes = ISZ(plusSettingsFilename))

      transpilerScripts = transpilerScripts + (id ~> trans)

    } // end slang type library

    val scripts: ISZ[(String, ST)] = transpilerScripts.entries.map((m: (String, (ST, TranspilerConfig))) => (m._1, m._2._1))
    transpilerOptions = transpilerOptions ++ transpilerScripts.values.map((m: (ST, TranspilerConfig)) => m._2)

    val slashTranspileScript = TranspilerTemplate.transpilerSel4Preamble(scripts.map(m => (m._1, m._2)))
    addExeResource(dirs.slangBinDir, ISZ("transpile-sel4.cmd"), slashTranspileScript, T)

  }

  def genGlobals(ports: ISZ[Port],
                 names: Names): ST = {
    val _ports: ISZ[ST] = ports.map(p => {
      val portComment = SeL4NixTemplate.portComment(p.name, p.feature.direction.string, p.feature.category.string, p.getPortTypeNames.qualifiedTypeName)
      SeL4NixTemplate.portVariable(names.bridgeIdentifier, p.sel4PortVarable, p.name, p.nameId, portComment)
    })
    return st"${(_ports, "\n\n")}"
  }

  def genDispatchStatus(names: Names,
                        ports: ISZ[Port],
                        value: Dispatch_Protocol.Type): ST = {
    val body: ST = value match {
      case Dispatch_Protocol.Periodic => st"return TimeTriggered()"
      case Dispatch_Protocol.Sporadic => {
        val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
        val checks: ISZ[ST] = inEventPorts.map(p => {
          val extObj_isEmptyMethodName = s"${names.sel4SlangExtensionName}.${genExtensionMethodName(p, "IsEmpty")}"
          st"""if(!${extObj_isEmptyMethodName}()) {
              |  portIds = portIds :+ ${p.nameId}
              |}"""
        })
        st"""var portIds: ISZ[Art.PortId] = ISZ()
            |${(checks, "\n")}
            |return EventTriggered(portIds)"""
      }
    }

    return SeL4NixTemplate.dispatchStatus(body)
  }

  def genReceiveInput(ports: ISZ[Port],
                      names: Names): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => CommonUtil.isInFeature(p.feature))
    val entries: ISZ[ST] = inPorts.map(p =>
      st"${p.sel4PortVarable} = ${names.sel4SlangExtensionName}.${genExtensionMethodName(p, "Receive")}()")

    return SeL4NixTemplate.receiveInput(st"${(entries, "\n\n")}")
  }

  def genPutValue(ports: ISZ[Port],
                  instanceName: String): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => CommonUtil.isOutFeature(p.feature))

    val options: ISZ[(ST, ST)] = outPorts.map(p => {
      val test: ST = st"portId == ${p.nameId}"
      val assign: ST = st"${p.sel4PortVarable} = Some(data)"
      (test, assign)
    })
    val optEls: ST = st"""halt(s"Unexpected: ${instanceName}.putValue called with: $${portId}")"""
    val ifelses = SeL4NixTemplate.ifEsleHelper(options, Some(optEls))

    return SeL4NixTemplate.putValue(ifelses)
  }

  def genGetValue(ports: ISZ[Port],
                  instanceName: String): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => CommonUtil.isInFeature(p.feature))

    val options: ISZ[(ST, ST)] = inPorts.map(p => {
      val test: ST = st"portId == ${p.nameId}"
      val assign: ST = st"return ${p.sel4PortVarable}"
      (test, assign)
    })
    val optEls: ST = st"""halt(s"Unexpected: ${instanceName}.getValue called with: $${portId}")"""
    val ifelses = SeL4NixTemplate.ifEsleHelper(options, Some(optEls))

    return SeL4NixTemplate.getValue(ifelses)
  }

  def genSendOutput(ports: ISZ[Port],
                    names: Names): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => CommonUtil.isOutFeature(p.feature))
    val entries: ISZ[ST] = outPorts.map(p => {
      st"""if(${p.sel4PortVarable}.nonEmpty) {
          |  ${names.sel4SlangExtensionName}.${genExtensionMethodName(p, "Send")}(${p.sel4PortVarable}.get)
          |  ${p.sel4PortVarable} = noData
          |}"""
    })
    return SeL4NixTemplate.sendOutput(st"${(entries, "\n\n")}")
  }

  def genExtensionMethodName(p: Port, methodName: String): String = {
    return s"${p.name}_${methodName}"
  }

  def genSlangSel4ExtensionObject(ports: ISZ[Port], names: Names): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if (CommonUtil.isInFeature(p.feature)) {
        st"""// returns T if seL4's ${p.name} port is empty, F otherwise 
            |def ${genExtensionMethodName(p, "IsEmpty")}(): B = $$
            |
            |// returns result of dequeuing seL4's ${p.name} port 
            |def ${genExtensionMethodName(p, "Receive")}(): Option[DataContent] = $$"""
      } else {
        st"""// send payload 'd' to components connected to seL4's ${p.name} port
            |def ${genExtensionMethodName(p, "Send")}(d: DataContent): Unit = $$"""
      }
    })
    return SeL4NixTemplate.extensionObject(names.packageName, names.sel4SlangExtensionName, st"${(entries, "\n\n")}")
  }

  def genSlangSel4ExtensionObjectStub(ports: ISZ[Port], packageName: String, names: Names): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if (CommonUtil.isInFeature(p.feature)) {
        st"""def ${genExtensionMethodName(p, "IsEmpty")}(): B = halt("stub")
            |
            |def ${genExtensionMethodName(p, "Receive")}(): Option[DataContent] = halt("stub")"""
      } else {
        st"""def ${genExtensionMethodName(p, "Send")}(d: DataContent): Unit = halt("stub")"""
      }
    })
    return SeL4NixTemplate.extensionObjectStub(
      names.packageName, names.sel4SlangExtensionStubName, st"${(entries, "\n\n")}")
  }

  def genTranspilerBase(basePackage: String,
                        instanceName: String,
                        identifier: String,
                        sourcePaths: ISZ[String],
                        cOutputDir: Os.Path,

                        maxSequenceSize: Z,

                        customSequenceSizes: ISZ[String],
                        customConstants: ISZ[String],
                        maxStackSizeInBytes: Z,

                        extensions: ISZ[Os.Path],
                        excludes: ISZ[String],

                        cmakeIncludes: ISZ[String]): (ST, TranspilerConfig) = {
    val packageName = s"${basePackage}/${instanceName}"
    val appName = s"${basePackage}.${instanceName}.${identifier}"

    val apps: ISZ[String] = ISZ(appName)
    val forwards: ISZ[String] = ISZ(s"art.ArtNative=${appName}")

    val buildApps = F

    val _sourcePaths = sourcePaths ++ ISZ(
      Util.pathAppend(dirs.mainDir, ISZ("art")),
      Util.pathAppend(dirs.mainDir, ISZ("data")),
      Util.pathAppend(dirs.seL4NixDir, ISZ(packageName)))

    val _extensions: Set[String] = Set.empty[String] ++ (extensions.map((m: Os.Path) => m.value) ++ arsitOptions.auxCodeDirs)

    return TranspilerTemplate.transpiler(
      verbose = arsitOptions.verbose,
      libraryName = instanceName,
      sourcepaths = _sourcePaths,
      outputDir = cOutputDir,
      binDir = dirs.slangBinDir,
      apps = apps,
      forwards = forwards,
      numBits = arsitOptions.bitWidth,
      maxSequenceSize = maxSequenceSize,
      maxStringSize = arsitOptions.maxStringSize,
      customArraySizes = customSequenceSizes,
      customConstants = customConstants,
      stackSizeInBytes = maxStackSizeInBytes,
      extensions = _extensions.elements,
      excludes = excludes,
      buildApps = buildApps,
      cmakeIncludes = cmakeIncludes
    )
  }

  def genTranspiler(basePackage: String,
                    names: Names,
                    maxStackSizeInBytes: Z,
                    numComponentInPorts: Z,
                    numComponentOutPorts: Z,
                    cOutputDir: Os.Path,
                    cExtensions: ISZ[Os.Path],
                    cmakeIncludes: ISZ[String]): (ST, TranspilerConfig) = {

    val components = symbolTable.airComponentMap.entries.filter(p =>
      CommonUtil.isThread(p._2) || (CommonUtil.isDevice(p._2) && arsitOptions.devicesAsThreads))

    val sourcePaths: ISZ[String] = ISZ(
      Util.pathAppend(dirs.mainDir, ISZ("bridge")),
      Util.pathAppend(dirs.mainDir, ISZ("component")),
      Util.pathAppend(dirs.seL4NixDir, names.path))

    val excludes: ISZ[String] = if (arsitOptions.excludeImpl) {
      components.map(c => {
        val componentNames: Names = Names(c._2, basePackage)
        s"${componentNames.packageName}.${componentNames.componentSingletonType}"
      })
    } else {
      ISZ()
    }

    val numComponentPorts: Z = numComponentInPorts + numComponentOutPorts

    var customSequenceSizes: ISZ[String] = ISZ(
      s"IS[Z,art.Bridge]=1",
      s"MS[Z,Option[art.Bridge]]=1",
      s"IS[Z,art.UPort]=${numComponentPorts}",
      s"IS[Z,art.UConnection]=1" // no connetions, but arg has to be > 0

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
      s"art.Art.maxComponents=1",
      s"art.Art.maxPorts=${numComponentPorts}"
    )

    // NOTE: the bridge entrypoints port sequences are dependent on the max
    // number of incoming and outgoing ports, so for now overestimate and just
    // use the total number of ports, or max array size if not cooked connections
    val maxSequenceSize: Z = getMaxSequenceSize(numComponentPorts, types)

    return genTranspilerBase(
      basePackage = basePackage,
      instanceName = names.componentSingletonType,
      identifier = names.identifier,
      sourcePaths = sourcePaths,
      cOutputDir = cOutputDir,

      maxSequenceSize = maxSequenceSize,

      customSequenceSizes = customSequenceSizes,
      customConstants = customConstants,
      maxStackSizeInBytes = maxStackSizeInBytes,

      extensions = cExtensions,
      excludes = excludes,

      cmakeIncludes = cmakeIncludes)
  }

  def genSel4Adapters(names: Names): ISZ[Os.Path] = {

    val root = Os.path(dirs.sel4EtcDir)

    var extensionFiles: ISZ[Os.Path] = ISZ()

    val fileName = names.cEntryPointAdapterName
    val macroName = StringUtil.toUpperCase(s"${fileName}_h")
    val implFile = root / "adapters" / names.componentSingletonType / s"${fileName}.c"
    val headerFile = root / "adapters" / names.componentSingletonType / s"${fileName}.h"

    var implMethods: ISZ[ST] = ISZ()
    var headerMethods: ISZ[ST] = ISZ()

    val methods: ISZ[(String, String)] = ISZ(
      ("initialiseArchitecture", "Unit"),
      ("initialiseEntryPoint", "Unit"),
      ("computeEntryPoint", "Unit"),
      ("entryPoints", "art_Bridge_EntryPoints")
    )

    for (entry <- methods) {
      val methodName = entry._1
      val returnType = entry._2
      val fullyQualifiedMethodName = s"${names.cEntryPointAdapterQualifiedName}_${methodName}"

      val signature = SeL4NixTemplate.methodSignature(fullyQualifiedMethodName, ISZ(), returnType)

      val routeToInstance = s"${names.basePackage}_${names.componentSingletonType}_${names.identifier}_${methodName}"

      val returnOpt: Option[String] = if (returnType == "Unit") None() else Some("return ")

      val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, implFile.name, "", fullyQualifiedMethodName, 0)

      implMethods = implMethods :+
        st"""${signature} {
            |  ${declNewStackFrame};
            |
            |  ${returnOpt}${routeToInstance}(SF_LAST);
            |}"""

      headerMethods = headerMethods :+ st"${signature};"
    }

    val impl = SeL4NixTemplate.cImplFile(fileName, implMethods, ISZ())
    val header = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

    addResource(implFile.up.value, ISZ(implFile.name), impl, T)
    addResource(headerFile.up.value, ISZ(headerFile.name), header, T)

    extensionFiles = (extensionFiles :+ headerFile) :+ implFile

    return extensionFiles
  }

  def getMaxSequenceSize(numPorts: Z, types: AadlTypes): Z = {

    val aadlArraySize: Z =
      if (!types.rawConnections)
        TypeUtil.findMaxAadlArraySize(types)
      else 0

    return CommonUtil.findMaxZ(ISZ(numPorts, aadlArraySize, arsitOptions.maxArraySize))
  }
}
