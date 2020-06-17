package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates.{ArchitectureTemplate, CMakeTemplate, SeL4NixTemplate, TranspilerTemplate}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names, StringUtil}
import org.sireum.hamr.codegen.common.symbols.{Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, DataTypeNames, TypeUtil}

case class SeL4NixGen(val dirs: ProjectDirectories,
                      val cExtensionDir: String,
                      val model: Aadl,
                      val arsitOptions: Cli.ArsitOption,
                      val symbolTable: SymbolTable,
                      val types: AadlTypes,
                      val previousPhase: Result) extends NixGen {

  val basePackage: String = arsitOptions.packageName

  var resources: ISZ[Resource] = ISZ()

  var transpilerOptions: ISZ[CTranspilerOption] = ISZ()

  val defaultMaxStackSizeInBytes: Z = z"16" * z"1024" * z"1024"

  val useArm: B = ops.ISZOps(symbolTable.getProcesses()).exists(p => p.toVirtualMachine())

  def generate(): ArsitResult = {
    assert(arsitOptions.platform == Cli.ArsitPlatform.SeL4)

    gen(model)

    return ArsitResult(
      previousPhase.resources() ++ resources,
      previousPhase.maxPort,
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

    val components: ISZ[Component] = symbolTable.airComponentMap.values.filter(p =>
      CommonUtil.isThread(p) || (CommonUtil.isDevice(p) && arsitOptions.devicesAsThreads))

    val rootCOutputDir: Os.Path = if(arsitOptions.outputCDir.nonEmpty) {
      Os.path(arsitOptions.outputCDir.get)
    } else {
      Os.path(dirs.srcDir) / "c/sel4"
    }

    var transpilerScripts: Map[String, (ST, CTranspilerOption)] = Map.empty
    val typeTouches: ISZ[ST] = genTypeTouches(types)

    for (component <- components) {

      val names: Names = Names(component, basePackage)

      val ports: ISZ[Port] = SlangUtil.getPorts(component, types, basePackage, z"0")

      val instanceName: String = names.instanceName

      val globals: ST = genGlobals(ports, names)

      val receiveInput: ST = genReceiveInput(ports, names)

      val putValue: ST = genPutValue(ports, names.identifier)

      val getValue: ST = genGetValue(ports, names.identifier)

      val sendOutput: ST = genSendOutput(ports, names)

      val period: Z = Util.getPeriod(component)

      val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(component).get
      val dispatchStatus = genDispatchStatus(names, ports, dispatchProtocol)

      val _ports = ports.map((p : Port) => ArchitectureTemplate.genPort(p))
      val _portArgs = ports.map((p : Port) => st"${p.name} = ${p.name}")

      val bridge = ArchitectureTemplate.bridge(
        names.bridgeIdentifier,
        names.instanceName,
        names.bridgeTypeName,
        z"0",
        ArchitectureTemplate.dispatchProtocol(dispatchProtocol, period),
        SlangUtil.getDispatchTriggers(component),
        _ports,
        _portArgs)

      val app = SeL4NixTemplate.app(
        basePackage,
        instanceName,
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
        typeTouches)

      addResource(
        dirs.seL4NixDir,
        ISZ(basePackage, instanceName, s"${names.identifier}.scala"),
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

      val cOutputDir: Os.Path = rootCOutputDir / instanceName

      val (paths, extResources) = genExtensionFiles(component, names, ports)
      resources = resources ++ extResources
      val transpilerExtensions: ISZ[Os.Path] = paths ++ genSel4Adapters(names) ++ getExistingCFiles(cExtensionDir)

      val stackSizeInBytes: Z = PropertyUtil.getStackSizeInBytes(component) match {
        case Some(size) => size
        case _ => defaultMaxStackSizeInBytes
      }

      val settingsFilename = s"${dirs.binDir}/${CMakeTemplate.cmake_settingsFilename(instanceName)}"
      addResource(settingsFilename, ISZ(), CMakeTemplate.cmake_sel4_settings_cmake(instanceName), F)

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
      
      transpilerScripts = transpilerScripts + (instanceName ~> trans)
    }
    
    { // Slang Type Library
      
      val id = "SlangTypeLibrary"
      val cOutputDir: Os.Path = rootCOutputDir / id
      val relPath = s"${cOutputDir.up.name}/${id}"
      
      val typeApp = SeL4NixTemplate.typeApp(basePackage, id, id, typeTouches)

      addResource(dirs.seL4NixDir,
        ISZ(basePackage, id, s"${id}.scala"),
        typeApp,
        T
      )

      var customSequenceSizes: ISZ[String] = ISZ()
      if(types.rawConnections) {
        // TODO is this necessary?
        TypeUtil.getMaxBitsSize(types) match {
          case Some(z) =>
            customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${z}"
          case _ => halt("Raw connections specified but couldn't determine max bit size")
        }
      }


      val settingsFilename = s"${dirs.binDir}/${CMakeTemplate.cmake_settingsFilename(id)}"
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

    val scripts = transpilerScripts.values.map(m => m._1)
    transpilerOptions = transpilerOptions ++ transpilerScripts.values.map(m => m._2)
    
    val transpileScript = TranspilerTemplate.transpilerScriptPreamble(scripts)

    addExeResource(dirs.binDir, ISZ("transpile-sel4.sh"), transpileScript, T)
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

  def genExtensionMethodName(p: Port, methodName: String): String = { return s"${p.name}_${methodName}" }
  
  def genSlangSel4ExtensionObject(ports: ISZ[Port], names: Names): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if(CommonUtil.isInFeature(p.feature)) {
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
      if(CommonUtil.isInFeature(p.feature)) {
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

                        cmakeIncludes: ISZ[String]): (ST, CTranspilerOption) = {
    val packageName = s"${basePackage}/${instanceName}"
    val appName = s"${basePackage}.${instanceName}.${identifier}"
    
    val apps: ISZ[String] = ISZ(appName)
    val forwards: ISZ[String] = ISZ(s"art.ArtNative=${appName}")

    val buildApps = F
      
    val _sourcePaths = sourcePaths ++ ISZ(
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("art")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("data")),
      SlangUtil.pathAppend(dirs.seL4NixDir, ISZ(packageName)))

    val _extensions: Set[String] = Set.empty ++ extensions.map(m => m.value)

    return TranspilerTemplate.transpiler(
      libraryName = instanceName,
      sourcepaths = _sourcePaths,
      outputDir = cOutputDir,
      binDir = dirs.binDir,
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
                    cmakeIncludes: ISZ[String]): (ST, CTranspilerOption) = {
     
    val components = symbolTable.airComponentMap.entries.filter(p =>
      CommonUtil.isThread(p._2) || (CommonUtil.isDevice(p._2) && arsitOptions.devicesAsThreads))

    val sourcePaths: ISZ[String] = ISZ(
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("bridge")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("component")),
      SlangUtil.pathAppend(dirs.seL4NixDir, names.path))
    
    val excludes:ISZ[String] = if(arsitOptions.excludeImpl) {
      for ((archVarName, m) <- components) yield {
        val componentNames: Names = Names(m, basePackage)
        s"${componentNames.packageName}.${componentNames.componentImpl}"
      }
    } else {
      ISZ()
    }

    val numComponentPorts: Z = numComponentInPorts + numComponentOutPorts

    var customSequenceSizes: ISZ[String] = ISZ(
      s"MS[Z,art.Bridge]=1",
      s"MS[Z,MOption[art.Bridge]]=1",
      s"IS[Z,art.UPort]=${numComponentPorts}",
      s"IS[Z,art.UConnection]=1" // no connetions, but arg has to be > 0

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
      s"art.Art.maxComponents=1",
      s"art.Art.maxPorts=${numComponentPorts}"
    )

    val maxPorts: Z = if(numComponentInPorts > numComponentOutPorts) numComponentInPorts else numComponentOutPorts

    // NOTE: the bridge entrypoints port sequences are dependent on the max
    // number of incoming or outgoing ports, so for now overestimate and just
    // use the total number of ports, or max array size if not cooked connections
    val maxSequenceSize: Z = getMaxSequenceSize(maxPorts, types)

    return genTranspilerBase(
      basePackage = basePackage,
      instanceName = names.instanceName,
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


  def genTypeTouches(types: AadlTypes): ISZ[ST] = {
    var a: ISZ[ST] = ISZ()
    var counter: Z = z"0"
    val _types: ISZ[AadlType] = if(types.rawConnections) {
      ISZ(TypeUtil.SlangEmbeddedBitType)
    } else {
      types.typeMap.entries.map((x : (String, AadlType)) => x._2)
    }

    for(typ <- _types) {
      //val typ = t._2
      val typeNames: DataTypeNames = SlangUtil.getDataTypeNames(typ, basePackage)
      a = a :+ SeL4NixTemplate.touchType(typeNames.qualifiedPayloadName, Some(typeNames.empty()))
      counter = counter + z"1"
    }
    a = a :+ SeL4NixTemplate.touchType("art.Empty", None())
    return a
  }

  def genSel4Adapters(names: Names) : ISZ[Os.Path] = {
    
    val root = Os.path(cExtensionDir)
    
    var extensionFiles: ISZ[Os.Path] = ISZ()
    
    val fileName = names.cEntryPointAdapterName
    val macroName = StringUtil.toUpperCase(s"${fileName}_h")
    val implFile = root / "adapters" / names.instanceName / s"${fileName}.c"
    val headerFile = root / "adapters" / names.instanceName / s"${fileName}.h"

    var implMethods: ISZ[ST] = ISZ()
    var headerMethods: ISZ[ST] = ISZ()

    val methods: ISZ[String] = ISZ("initialiseArchitecture", "initialiseEntryPoint", "computeEntryPoint")
    for(m <- methods) {
      val methodName = s"${names.cEntryPointAdapterQualifiedName}_${m}"

      val signature = SeL4NixTemplate.methodSignature(methodName, None(), ISZ(), "Unit")

      val route = s"${names.basePackage}_${names.instanceName}_${names.identifier}_${m}"

      implMethods = implMethods :+ st"""${signature} {
                                       |  ${route}(SF_LAST);
                                       |}"""

      headerMethods = headerMethods :+ st"${signature};"
    }

    val impl = SeL4NixTemplate.cImplFile(fileName, implMethods)
    val header = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

    addResource(implFile.up.value, ISZ(implFile.name), impl, T)
    addResource(headerFile.up.value, ISZ(headerFile.name), header, T)

    extensionFiles = (extensionFiles :+ headerFile) :+ implFile
    
    return extensionFiles
  } 

  def getMaxSequenceSize(numPorts: Z, types: AadlTypes): Z = {

    val aadlArraySize: Z =
      if(!types.rawConnections)
        TypeUtil.findMaxAadlArraySize(types)
      else 0

    return CommonUtil.findMaxZ(ISZ(numPorts, aadlArraySize, arsitOptions.maxArraySize))
  }
}
