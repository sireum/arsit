package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.codegen.common.{AadlType, AadlTypes, CommonUtil, DataTypeNames, Dispatch_Protocol, Names, PropertyUtil, StringUtil, SymbolTable, TypeUtil}

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

  val maxStackSize: Z = z"16" * z"1024" * z"1024"
  
  var maxSequenceSize: Z = 1

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

    maxSequenceSize = getMaxSequenceSize(components, types)
    
    val rootCOutputDir: Os.Path = if(arsitOptions.outputCDir.nonEmpty) {
      Os.path(arsitOptions.outputCDir.get)
    } else {
      Os.path(dirs.srcDir) / "c/sel4"
    }
    
    var transpilerScripts: Map[String, (ST, CTranspilerOption)] = Map.empty
    var sel4CompileScripts: ISZ[ST] = ISZ()
    val typeTouches: ISZ[ST] = genTypeTouches(types)
    
    for (component <- components) {

      val names: Names = Names(component, basePackage)

      val ports: ISZ[Port] = Util.getPorts(component, types, basePackage, z"0")

      val instanceName: String = names.instanceName

      val globals: ST = genGlobals(ports, names)

      val receiveInput: ST = genReceiveInput(ports, names)

      val putValue: ST = genPutValue(ports, names.identifier)

      val getValue: ST = genGetValue(ports, names.identifier)

      val sendOutput: ST = genSendOutput(ports, names)

      val period: Z = Util.getPeriod(component)

      val dispatchProtocol: Dispatch_Protocol.Type = PropertyUtil.getDispatchProtocol(component).get
      val dispatchStatus = genDispatchStatus(names, ports, dispatchProtocol)

      val _ports = ports.map((p : Port) => ArchTemplate.genPort(p))
      val _portArgs = ports.map((p : Port) => st"${p.name} = ${p.name}")

      val bridge = ArchTemplate.bridge(
        names.bridgeIdentifier,
        names.instanceName,
        names.bridgeTypeName,
        z"0",
        ArchTemplate.dispatchProtocol(dispatchProtocol, period),
        Util.getDispatchTriggers(component),
        _ports,
        _portArgs)

      val app = NixTemplate.app(
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

      val relPath = s"${cOutputDir.up.name}/${instanceName}"
      sel4CompileScripts = sel4CompileScripts :+ TranspilerTemplate.compileLib(relPath)

      val (paths, extResources) = genExtensionFiles(component, names, ports)
      resources = resources ++ extResources
      val transpilerExtensions: ISZ[Os.Path] = paths ++ genSel4Adapters(names) ++ getExistingCFiles(cExtensionDir)

      val stackSize: Z = PropertyUtil.getStackSizeInBytes(component) match {
        case Some(size) => size
        case _ => maxStackSize
      }
      
      val trans = genTranspiler(
        basePackage = basePackage,
        names = names, 
        maxStackSize = stackSize,
        numComponentPorts = ports.size,
        cOutputDir = cOutputDir,
        cExtensions = transpilerExtensions)
      
      transpilerScripts = transpilerScripts + (instanceName ~> trans)
    }
    
    { // Slang Type Library
      
      val id = "SlangTypeLibrary"
      val cOutputDir: Os.Path = rootCOutputDir / id
      val relPath = s"${cOutputDir.up.name}/${id}"
      
      val typeApp = NixTemplate.typeApp(basePackage, id, id, typeTouches)

      addResource(dirs.seL4NixDir,
        ISZ(basePackage, id, s"${id}.scala"),
        typeApp,
        T
      )
      sel4CompileScripts = sel4CompileScripts :+ TranspilerTemplate.compileLib(relPath)

      var customSequenceSizes: ISZ[String] = ISZ()
      if(types.rawConnections) {
        // TODO is this necessary?
        getMaxBitsSize() match {
          case Some(z) =>
            customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${z}"
          case _ => halt("Raw connections specified but couldn't determine max bit size")
        }
      }

      val trans = genTranspilerBase(
        basePackage = basePackage,
        instanceName = id,
        identifier = id,
        sourcePaths = ISZ(),
        cOutputDir = cOutputDir,
          
        customSequenceSizes = customSequenceSizes,
        customConstants = ISZ(),
        maxStackSize = maxStackSize,
        
        extensions = ISZ(),        
        excludes = ISZ())
      
      transpilerScripts = transpilerScripts + (id ~> trans)
    }
    
    val sel4CompileScript = TranspilerTemplate.compileLibPreamble(sel4CompileScripts)
    addExeResource(rootCOutputDir.value, ISZ("..", "bin", "compile-hamr-lib.sh"), sel4CompileScript, T)
    
    val scripts = transpilerScripts.values.map(m => m._1)
    transpilerOptions = transpilerOptions ++ transpilerScripts.values.map(m => m._2)
    
    val transpileScript = TranspilerTemplate.transpilerScriptPreamble(scripts)

    addExeResource(dirs.binDir, ISZ("transpile-sel4.sh"), transpileScript, T)
  }
  
  def genGlobals(ports: ISZ[Port],
                 names: Names): ST = {
    val _ports: ISZ[ST] = ports.map(p => {
      val portComment = NixTemplate.portComment(p.name, p.feature.direction.string, p.feature.category.string, p.getPortTypeNames.qualifiedTypeName)
      NixTemplate.portVariable(names.bridgeIdentifier, p.sel4PortVarable, p.name, p.nameId, portComment)
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
    
    return NixTemplate.dispatchStatus(body)
  }

  def genReceiveInput(ports: ISZ[Port],
                      names: Names): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => CommonUtil.isInFeature(p.feature))
    val entries: ISZ[ST] = inPorts.map(p => 
      st"${p.sel4PortVarable} = ${names.sel4SlangExtensionName}.${genExtensionMethodName(p, "Receive")}()")
    
    return NixTemplate.receiveInput(st"${(entries, "\n\n")}")
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
    val ifelses = NixTemplate.ifEsleHelper(options, Some(optEls))
    
    return NixTemplate.putValue(ifelses)
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
    val ifelses = NixTemplate.ifEsleHelper(options, Some(optEls))

    return NixTemplate.getValue(ifelses)
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
    return NixTemplate.sendOutput(st"${(entries, "\n\n")}")
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
    return NixTemplate.extensionObject(names.packageName, names.sel4SlangExtensionName, st"${(entries, "\n\n")}")
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
    return NixTemplate.extensionObjectStub(
      names.packageName, names.sel4SlangExtensionStubName, st"${(entries, "\n\n")}")
  }

  def genTranspilerBase(basePackage: String,
                        instanceName: String,
                        identifier: String,
                        sourcePaths: ISZ[String],
                        cOutputDir: Os.Path,

                        customSequenceSizes: ISZ[String],
                        customConstants: ISZ[String],
                        maxStackSize: Z,

                        extensions: ISZ[Os.Path],
                        excludes: ISZ[String]): (ST, CTranspilerOption) = {
    val packageName = s"${basePackage}/${instanceName}"
    val appName = s"${basePackage}.${instanceName}.${identifier}"
    
    val apps: ISZ[String] = ISZ(appName)
    val forwards: ISZ[String] = ISZ(s"art.ArtNative=${appName}")

    val buildApps = F
    val additionalInstructions: Option[ST] = Some(st"""FILE=$${OUTPUT_DIR}/CMakeLists.txt
                                                      |echo -e "\n\nadd_definitions(-DCAMKES)" >> $$FILE""")
      
    val _sourcePaths = sourcePaths ++ ISZ(
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("art")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("data")),
      SlangUtil.pathAppend(dirs.seL4NixDir, ISZ(packageName)))

    val _extensions: Set[String] = Set.empty ++ extensions.map(m => m.value)

    return TranspilerTemplate.transpiler(
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
      stackSizeInBytes = maxStackSize,
      extensions = _extensions.elements,
      excludes = excludes,
      buildApps = buildApps,
      additionalInstructions = additionalInstructions
    )
  }

  def genTranspiler(basePackage: String,
                    names: Names,
                    maxStackSize: Z,
                    numComponentPorts: Z,
                    cOutputDir: Os.Path,
                    cExtensions: ISZ[Os.Path]): (ST, CTranspilerOption) = {
     
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
    
    var customSequenceSizes: ISZ[String] = ISZ(
      s"MS[Z,art.Bridge]=1",
      s"MS[Z,MOption[art.Bridge]]=1",
      s"IS[Z,art.UPort]=${numComponentPorts}",
      s"IS[Z,art.UConnection]=1" // no connetions, but arg has to be > 0

      // not valid
      //s"MS[org.sireum.Z,org.sireum.Option[art.UPort]]=${maxPortsForComponents}"
    )

    if(types.rawConnections) {
      getMaxBitsSize() match {
        case Some(z) =>
          customSequenceSizes = customSequenceSizes :+ s"IS[Z,B]=${z}"
        case _ => halt("Raw connections specified but couldn't determine max bit size")
      }
    }

    val customConstants: ISZ[String] = ISZ(
      s"art.Art.maxComponents=1",
      s"art.Art.maxPorts=${numComponentPorts}"
    )
    
    return genTranspilerBase(
      basePackage = basePackage,
      instanceName = names.instanceName,
      identifier = names.identifier,
      sourcePaths = sourcePaths,
      cOutputDir = cOutputDir,
      
      customSequenceSizes = customSequenceSizes,
      customConstants = customConstants,
      maxStackSize = maxStackSize,

      extensions = cExtensions,
      excludes = excludes)
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
      a = a :+ NixTemplate.touchType(typeNames.qualifiedPayloadName, Some(typeNames.empty()))
      counter = counter + z"1"
    }
    a = a :+ NixTemplate.touchType("art.Empty", None())
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

      val signature = NixTemplate.methodSignature(methodName, None(), ISZ(), "Unit")

      val route = s"${names.basePackage}_${names.instanceName}_${names.identifier}_${m}"

      implMethods = implMethods :+ st"""${signature} {
                                       |  ${route}(SF_LAST);
                                       |}"""

      headerMethods = headerMethods :+ st"${signature};"
    }

    val impl = NixTemplate.cImplFile(fileName, implMethods)
    val header = NixTemplate.cHeaderFile(macroName, headerMethods)

    addResource(implFile.up.value, ISZ(implFile.name), impl, T)
    addResource(headerFile.up.value, ISZ(headerFile.name), header, T)

    extensionFiles = (extensionFiles :+ headerFile) :+ implFile
    
    return extensionFiles
  } 

  def getMaxSequenceSize(components: ISZ[Component], types: AadlTypes): Z = {
    var max:Z = z"0"

    for(c <- components) {
      val numPorts = Util.getPorts(c, types, basePackage, z"0").size
      if(numPorts > max) {
        max = numPorts
      }
    }

    val aadlArraySize = TypeUtil.findMaxAadlArraySize(types)
    if(aadlArraySize > max) {
      max = aadlArraySize
    }

    return CommonUtil.findMaxZ(ISZ(max, aadlArraySize, arsitOptions.maxArraySize))
  }
}
