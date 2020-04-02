package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.Util.reporter

class SeL4NixGen(dirs: ProjectDirectories,
                m: Aadl,
                arsitOptions: Cli.ArsitOption,
                types: AadlTypes,
                previousPhase: Result) {
  
  var cExtensionDir : String = _

  var cUserExtensionDir : Option[String] = None()

  val basePackage: String = arsitOptions.packageName

  var componentMap : HashMap[String, Component] = HashMap.empty

  var systemImplmentation: Component = _

  var resources: ISZ[Resource] = ISZ()

  var transpilerOptions: ISZ[CTranspilerOption] = ISZ()

  val maxStackSize: Z = z"16" * z"1024" * z"1024"
  
  var maxSequenceSize: Z = 1
  
  // port-paths -> connInstances
  var inConnections: HashMap[String, ISZ[ConnectionInstance]] = HashMap.empty
  var outConnections: HashMap[String, ISZ[ConnectionInstance]] = HashMap.empty

  def generator(): ArsitResult = {
    assert(arsitOptions.platform == Cli.ArsitPlatform.SeL4)

    cExtensionDir = if (arsitOptions.auxCodeDir.nonEmpty) {
      assert(arsitOptions.auxCodeDir.size == 1)
      arsitOptions.auxCodeDir(0)
    } else {
      SlangUtil.pathAppend(dirs.srcDir, ISZ("c", "ext-c"))
    }

    if (arsitOptions.auxCodeDir.nonEmpty) {
      assert(arsitOptions.auxCodeDir.size == 1)
      cUserExtensionDir = Some(arsitOptions.auxCodeDir(0))
    }

    gen(m)

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
    assert(Util.isSystem(model.components(0)))
    systemImplmentation = model.components(0)

    resolve(model)

    val components: ISZ[Component] = componentMap.values.filter(p =>
      Util.isThread(p) || (Util.isDevice(p) && arsitOptions.devicesAsThreads))

    maxSequenceSize = getMaxSequenceSize(components, types)
    
    val rootCOutputDir: Os.Path = if(arsitOptions.outputCDir.nonEmpty) {
      Os.path(arsitOptions.outputCDir.get)
    } else {
      Os.path(dirs.srcDir) / "c/sel4"
    }
    
    var transpilerScripts: Map[String, (ST, CTranspilerOption)] = Map.empty
    var sel4CompileScripts: ISZ[ST] = ISZ()
    val typeTouches: ISZ[ST] = genTypeTouches(types)
    
    for (m <- components) {

      val names: Names = Util.getComponentNames(m, basePackage)

      val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, z"0")
      
      val dispatchStatus = genDispatchStatus(names, ports, Util.getSlangEmbeddedDispatchProtocol(m))

      val instanceName: String = names.instanceName

      val globals: ST = genGlobals(ports, names)

      val receiveInput: ST = genReceiveInput(ports, names)

      val putValue: ST = genPutValue(ports, names.identifier)

      val getValue: ST = genGetValue(ports, names.identifier)

      val sendOutput: ST = genSendOutput(ports, names)

      val period: Z = Util.getPeriod(m)
      val dispatchProtocol: DispatchProtocol.Type = Util.getDispatchProtocol(m).get

      val bridge = ArchTemplate.bridge(
        names.bridgeIdentifier,
        names.instanceName,
        names.bridgeTypeName,
        z"0",
        ArchTemplate.dispatchProtocol(dispatchProtocol, period),
        Util.getDispatchTriggers(m),
        ports)

      val app = Sel4NixTemplate.app(
        basePackage,
        instanceName,
        ISZ(s"import ${names.basePackage}._", s"import ${names.packageName}.${names.sel4ExtensionName}"),
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
        
        val extensionObject: ST = genExtensionObject(ports, names)

        addResource(
          dirs.seL4NixDir,
          ISZ(names.packagePath, s"${names.sel4ExtensionName}.scala"),
          extensionObject,
          T)

        val extensionObjectStub: ST = genExtensionObjectStub(ports, basePackage, names)

        addResource(
          dirs.seL4NixDir,
          ISZ(names.packagePath, s"${names.sel4ExtensionStubName}.scala"),
          extensionObjectStub,
          T)
      }
      
      val cOutputDir: Os.Path = rootCOutputDir / instanceName

      val relPath = s"${cOutputDir.up.name}/${instanceName}"
      sel4CompileScripts = sel4CompileScripts :+ TranspilerTemplate.compileLib(relPath)

      val exts: ISZ[Os.Path] = genExtensionFiles(m, names, ports)

      val stackSize: Z = Util.getStackSizeInBytes(m) match {
        case Some(size) => size
        case _ => maxStackSize
      }
      
      val trans = genTranspiler(
        basePackage = basePackage,
        names = names, 
        maxStackSize = stackSize,
        numComponentPorts = ports.size,
        cOutputDir = cOutputDir,
        cExtensions = exts)
      
      transpilerScripts = transpilerScripts + (instanceName ~> trans)
    }
    
    { // Slang Type Library
      
      val id = "SlangTypeLibrary"
      //val dataDir = "data"
      val cOutputDir: Os.Path = rootCOutputDir / id
      val relPath = s"${cOutputDir.up.name}/${id}"
      
      val typeApp = Sel4NixTemplate.typeApp(basePackage, id, id, typeTouches)

      addResource(dirs.seL4NixDir,
        ISZ(basePackage, id, s"${id}.scala"),
        typeApp,
        T
      )
      sel4CompileScripts = sel4CompileScripts :+ TranspilerTemplate.compileLib(relPath)
            
      val trans = genTranspilerBase(
        basePackage = basePackage,
        instanceName = id,
        identifier = id,
        sourcePaths = ISZ(),
        cOutputDir = cOutputDir,
          
        customSequenceSizes = ISZ(), 
        customConstants = ISZ(),
        maxStackSize = maxStackSize,
        
        extensions = ISZ(),        
        excludes = ISZ())
      
      transpilerScripts = transpilerScripts + (id ~> trans)
    }
    
    val sel4CompileScript = TranspilerTemplate.compileLibPreamble(sel4CompileScripts)
    addExeResource(rootCOutputDir.value, ISZ("..", "compile-hamr-lib.sh"), sel4CompileScript, T)
    
    val scripts = transpilerScripts.values.map(m => m._1)
    transpilerOptions = transpilerOptions ++ transpilerScripts.values.map(m => m._2)
    
    val transpileScript = TranspilerTemplate.transpilerScriptPreamble(scripts)

    addExeResource(dirs.binDir, ISZ("transpile-sel4.sh"), transpileScript, T)
  }
  
  def genGlobals(ports: ISZ[Port],
                 names: Names): ST = {
    val _ports: ISZ[ST] = ports.map(p => {
      val portComment = Sel4NixTemplate.portComment(p.name, p.feature.direction.string, p.feature.category.string, p.portType.qualifiedTypeName)
      Sel4NixTemplate.portVariable(names.bridgeIdentifier, p.sel4PortVarable, p.name, p.nameId, portComment)
    })
    return st"${(_ports, "\n\n")}"
  }

  def genDispatchStatus(names: Names,
                        ports: ISZ[Port],
                        value: DispatchProtocol.Type): ST = {
    val body: ST = value match {
      case DispatchProtocol.Periodic => st"return TimeTriggered()"
      case DispatchProtocol.Sporadic => {
        val inEventPorts = ports.filter(p => Util.isInFeature(p.feature) && Util.isEventPort(p.feature))
        val checks: ISZ[ST] = inEventPorts.map(p => {
          val extObj_isEmptyMethodName = s"${names.sel4ExtensionName}.${genExtensionMethodName(p, "IsEmpty")}"
          st"""if(!${extObj_isEmptyMethodName}()) {
              |  portIds = portIds :+ ${p.nameId}
              |}"""
        })
        st"""var portIds: ISZ[Art.PortId] = ISZ()
            |${(checks, "\n")}
            |return EventTriggered(portIds)"""
      }
    }
    
    return Sel4NixTemplate.dispatchStatus(body)
  }

  def genReceiveInput(ports: ISZ[Port],
                      names: Names): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => Util.isInFeature(p.feature))
    val entries: ISZ[ST] = inPorts.map(p => 
      st"${p.sel4PortVarable} = ${names.sel4ExtensionName}.${genExtensionMethodName(p, "Receive")}()")
    
    return Sel4NixTemplate.receiveInput(st"${(entries, "\n\n")}")
  }
  
  def genPutValue(ports: ISZ[Port],
                  instanceName: String): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => Util.isOutFeature(p.feature))
    
    val options: ISZ[(ST, ST)] = outPorts.map(p => {
      val test: ST = st"portId == ${p.nameId}" 
      val assign: ST = st"${p.sel4PortVarable} = Some(data)"
      (test, assign)
    })
    val optEls: ST = st"""halt(s"Unexpected: ${instanceName}.putValue called with: $${portId}")"""
    val ifelses = Sel4NixTemplate.ifEsleHelper(options, Some(optEls))
    
    return Sel4NixTemplate.putValue(ifelses)
  }
  
  def genGetValue(ports: ISZ[Port],
                  instanceName: String): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => Util.isInFeature(p.feature))

    val options: ISZ[(ST, ST)] = inPorts.map(p => {
      val test: ST = st"portId == ${p.nameId}"
      val assign: ST = st"return ${p.sel4PortVarable}"
      (test, assign)
    })
    val optEls: ST = st"""halt(s"Unexpected: ${instanceName}.getValue called with: $${portId}")"""
    val ifelses = Sel4NixTemplate.ifEsleHelper(options, Some(optEls))

    return Sel4NixTemplate.getValue(ifelses)
  }
  
  def genSendOutput(ports: ISZ[Port],
                    names: Names): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => Util.isOutFeature(p.feature))
    val entries: ISZ[ST] = outPorts.map(p => {
      st"""if(${p.sel4PortVarable}.nonEmpty) {
          |  ${names.sel4ExtensionName}.${genExtensionMethodName(p, "Send")}(${p.sel4PortVarable}.get)
          |  ${p.sel4PortVarable} = noData
          |}"""
    })
    return Sel4NixTemplate.sendOutput(st"${(entries, "\n\n")}")
  }

  def genExtensionMethodName(p: Port, methodName: String): String = { return s"${p.name}_${methodName}" }
  
  def genExtensionObject(ports: ISZ[Port], names: Names): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if(Util.isInFeature(p.feature)) {
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
    return Sel4NixTemplate.extensionObject(names.packageName, names.sel4ExtensionName, st"${(entries, "\n\n")}")
  }
  
  def genExtensionObjectStub(ports: ISZ[Port], packageName: String, names: Names): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if(Util.isInFeature(p.feature)) {
        st"""def ${genExtensionMethodName(p, "IsEmpty")}(): B = halt("stub")
            |
            |def ${genExtensionMethodName(p, "Receive")}(): Option[DataContent] = halt("stub")"""
      } else {
        st"""def ${genExtensionMethodName(p, "Send")}(d: DataContent): Unit = halt("stub")"""
      }
    })
    return Sel4NixTemplate.extensionObjectStub(
      names.packageName, names.sel4ExtensionStubName, st"${(entries, "\n\n")}")
  }

  def genTranspilerBase(basePackage: String,
                        instanceName: String,
                        identifier: String,
                        sourcePaths: ISZ[String],
                        cOutputDir: Os.Path,

                        customSequenceSizes: ISZ[String],
                        customConstants: ISZ[String],
                        maxStackSize: Z,

                        extensions: ISZ[String],
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
      extensions = extensions,
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
     
    val components = componentMap.entries.filter(p =>
      Util.isThread(p._2) || (Util.isDevice(p._2) && arsitOptions.devicesAsThreads))

    val sourcePaths: ISZ[String] = ISZ(
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("bridge")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("component")),
      SlangUtil.pathAppend(dirs.seL4NixDir, names.path))
    
    val excludes:ISZ[String] = if(arsitOptions.excludeImpl) {
      for ((archVarName, m) <- components) yield {
        val componentNames: Names = Util.getComponentNames(m, basePackage)
        s"${componentNames.packageName}.${componentNames.componentImpl}"
      }
    } else {
      ISZ()
    }

    val extensions: ISZ[String] = cExtensions.map(p => p.value) //ISZ(s"${cExtensionDir}/ext.c", s"${cExtensionDir}/ext.h")
    
    val customSequenceSizes: ISZ[String] = ISZ(
      s"MS[Z,art.Bridge]=1",
      s"MS[Z,MOption[art.Bridge]]=1",
      s"IS[Z,art.UPort]=${numComponentPorts}",
      s"IS[Z,art.UConnection]=1" // no connetions, but arg has to be > 0

      // not valid
      //s"MS[org.sireum.Z,org.sireum.Option[art.UPort]]=${maxPortsForComponents}"
    )

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

      extensions = extensions,
      excludes = excludes)
  }


  def genTypeTouches(types: AadlTypes): ISZ[ST] = {
    var a: ISZ[ST] = ISZ()
    var counter: Z = z"0"
    for(t <- types.typeMap.entries) {
      val typ = t._2
      val typeNames: DataTypeNames = SlangUtil.getDataTypeNames(typ, basePackage)
      a = a :+ Sel4NixTemplate.touchType(typeNames.qualifiedPayloadName, Some(typeNames.empty()))
      counter = counter + z"1"
    }
    a = a :+ Sel4NixTemplate.touchType("art.Empty", None())
    return a
  }

  def genStubInitializeMethod(names: Names, ports: ISZ[Port]): ST = {
    val preParams = Some(st"STACK_FRAME")
    val params: ISZ[ST] = ISZ(st"${names.cComponentImplQualifiedName} this")
    val initialiseMethod = Sel4NixTemplate.methodSignature(s"${names.cComponentImplQualifiedName}_initialise_", preParams, params, "Unit")
    
    val loggers: ISZ[String] = ISZ("logInfo", "logDebug", "logError")
    val statements = loggers.map(l => {
      val mname = Sel4NamesUtil.apiHelperMethodName(l, names)
      st"""${mname}(this, string("Example ${l}"));"""
    })
    val ret: ST = st"""${initialiseMethod} {
                      | // example api usage
                      |  
                      | ${(statements, "\n")}
                      |}"""
    return ret
  }

  def genExtensionFiles(c: Component, names: Names, ports: ISZ[Port]): ISZ[Os.Path] = {
    val root = Os.path(cExtensionDir)
      
    var extensionFiles: ISZ[Os.Path] = ISZ()
    
    val extC = root / "ext.c"
    val extH = root / "ext.h"
    addResource(extC.up.value, ISZ(extC.name), SlangUtil.getLibraryFile("ext.c"), F)
    addResource(extH.up.value, ISZ(extH.name), SlangUtil.getLibraryFile("ext.h"), F)
    
    extensionFiles = (extensionFiles :+ extC) :+ extH
    
    if(arsitOptions.excludeImpl) {

      val componentName = names.cComponentImplQualifiedName

      var headerMethods: ISZ[ST] = ISZ()
      var implMethods: ISZ[ST] = ISZ()
      for(p <- ports) {
        val typeNames = SlangUtil.getDataTypeNames(p._portType, basePackage)
          
        var params = ISZ(st"${componentName} this")
        
        if(Util.isInFeature(p.feature)) {
          
          val methodName = Sel4NamesUtil.apiHelperMethodName(s"get_${p.name}", names)
          val returnType = "B"
          val pointer: String = if(typeNames.isEnum() || typeNames.isBaseType()) "*" else ""
          params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"

          val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, returnType)
          val apiGetMethodName = s"${names.cBridgeApi}_get${p.name}_"

          headerMethods = headerMethods :+ st"${signature};"

          implMethods = implMethods :+ Sel4NixTemplate.apiGet(
            signature, 
            apiGetMethodName, 
            names.cThisApi,
            typeNames)
          
        } else {
          val isEventPort = p.feature.category == FeatureCategory.EventPort

          val methodName = Sel4NamesUtil.apiHelperMethodName(s"send_${p.name}", names)
          val returnType = "Unit"
          val sendSet: String = if(Util.isDataPort(p.feature)) "set" else "send"
          if(!isEventPort) {
            params = params :+ st"${typeNames.qualifiedCTypeName} value"
          }

          val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, returnType)
          val apiSetMethodName = s"${names.cBridgeApi}_${sendSet}${p.name}_"
          
          headerMethods = headerMethods :+ st"${signature};"
          implMethods = implMethods :+ Sel4NixTemplate.apiSet(signature, apiSetMethodName, names.cThisApi, isEventPort)
        }
      }

      { // logging methods
        
        val loggers = ISZ("logInfo", "logDebug", "logError")
        
        for(l <- loggers) {
          val methodName = Sel4NamesUtil.apiHelperMethodName(l, names)
          val params = ISZ(st"${componentName} this", st"String str")

          val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, "Unit")
          
          val apiLogMethodName = s"${names.cBridgeApi}_${l}_"

          headerMethods = headerMethods :+ st"${signature};"
          implMethods = implMethods :+ Sel4NixTemplate.apiLog(signature, apiLogMethodName, names.cThisApi)
        }
      }
      
      
      val extRoot = root / names.componentImpl
      val apiHeaderName = s"${names.componentImpl}_api"

      val headerApiFile = extRoot / s"${apiHeaderName}.h"
      val implApiFile =  extRoot / s"${apiHeaderName}.c"
      
      val macroName = SlangUtil.toUpperCase(s"${apiHeaderName}_h")
            
      val headerContents = Sel4NixTemplate.cHeaderFile(macroName, headerMethods)
      
      val implContents = Sel4NixTemplate.cImplFile(apiHeaderName, implMethods)

      extensionFiles = extensionFiles :+ headerApiFile
      extensionFiles = extensionFiles :+ implApiFile

      addResource(headerApiFile.up.value, ISZ(headerApiFile.name), headerContents, T)
      addResource(implApiFile.up.value, ISZ(implApiFile.name), implContents, T)
      
      val preParams: Option[ST] = Some(st"STACK_FRAME")
      val params: ISZ[ST] = ISZ(st"${componentName} this")

      { // add entrypoint stubs
        
        var methods : ISZ[ST] = ISZ(genStubInitializeMethod(names, ports))
        
        Util.getDispatchProtocol(c) match {
          case Some(DispatchProtocol.Periodic) =>
            // timetriggered
            val timeTriggered = Sel4NixTemplate.methodSignature(s"${componentName}_timeTriggered_", preParams, params, "Unit")
            methods = methods :+ st"${timeTriggered} {}"
            
          case Some(DispatchProtocol.Sporadic) =>
            val inEventPorts = ports.filter(f => Util.isEventPort(f.feature) && Util.isInFeature(f.feature))
            
            for(p <- inEventPorts) {
              val handlerName = s"${componentName}_handle${p.name}"
              var eventDataParams = params
              if(p.feature.category == FeatureCategory.EventDataPort) {
                val typeNames = SlangUtil.getDataTypeNames(p._portType, basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handler = Sel4NixTemplate.methodSignature(s"${handlerName}_", preParams, eventDataParams, "Unit")
              val logInfo = Sel4NamesUtil.apiHelperMethodName("logInfo", names);
              methods = methods :+ st"""${handler} {
                                       |  
                                       |  DeclNewString(${p.name}String);
                                       |  String__append((String) &${p.name}String, string("${handlerName} called"));
                                       |  ${logInfo} (this, (String) &${p.name}String);
                                       |}"""               
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }
        
        val impl = st"""#include <${apiHeaderName}.h>
                       |#include <ext.h>
                       |
                       |${(methods, "\n\n")}
                       |"""

        val implFile = extRoot / s"${names.componentImpl}.c"
        extensionFiles = extensionFiles :+ implFile
        addResource(implFile.up.value, ISZ(implFile.name), impl, F)
      }
    }
    
    { // adapters
      val fileName = names.cEntryPointAdapterName
      val macroName = SlangUtil.toUpperCase(s"${fileName}_h")
      val implFile = root / "adapters" / names.instanceName / s"${fileName}.c"
      val headerFile = root / "adapters" / names.instanceName / s"${fileName}.h"
      
      var implMethods: ISZ[ST] = ISZ()
      var headerMethods: ISZ[ST] = ISZ()
      
      val methods: ISZ[String] = ISZ("initialiseArchitecture", "initialiseEntryPoint", "computeEntryPoint")
      for(m <- methods) {
        val methodName = s"${names.cEntryPointAdapterQualifiedName}_${m}"
        
        val signature = Sel4NixTemplate.methodSignature(methodName, None(), ISZ(), "Unit")
        
        val route = s"${names.basePackage}_${names.instanceName}_${names.identifier}_${m}"
        
        implMethods = implMethods :+ st"""${signature} {
                                         |  ${route}(SF_LAST);
                                         |}"""
        
        headerMethods = headerMethods :+ st"${signature};"
      }
      
      val impl = Sel4NixTemplate.cImplFile(fileName, implMethods)
      val header = Sel4NixTemplate.cHeaderFile(macroName, headerMethods)

      addResource(implFile.up.value, ISZ(implFile.name), impl, T)
      addResource(headerFile.up.value, ISZ(headerFile.name), header, T)
      
      extensionFiles = (extensionFiles :+ headerFile) :+ implFile
    }
    
    return extensionFiles
  }
  
  def resolve(model: Aadl): B = {
    var connections: ISZ[ConnectionInstance] = ISZ()
    
    // build component map
    def r(c: Component): Unit = {
      assert(!componentMap.contains(Util.getName(c.identifier)))
      componentMap += (Util.getName(c.identifier) → c)
      connections = connections ++ c.connectionInstances
      for (s <- c.subComponents) r(s)
    }
    for (c <- model.components) r(c)

    val threadConnections: ISZ[ConnectionInstance] = connections.filter(ci => {
      val srcC = componentMap.get(Util.getName(ci.src.component)).get
      val dstC = componentMap.get(Util.getName(ci.dst.component)).get
      Util.isThread(srcC) && Util.isThread(dstC)
    })

    for(ci <- threadConnections) {
      def add(portPath: String, isIn: B): Unit = {
        val map: HashMap[String, ISZ[ConnectionInstance]] = isIn match {
          case T => inConnections
          case F => outConnections
        }
        var cis: ISZ[ConnectionInstance] = map.get(portPath) match {
          case Some(x) => x
          case _ => ISZ()
        }
        cis = cis :+ ci
        isIn match {
          case T => inConnections = inConnections + (portPath ~> cis)
          case F => outConnections = outConnections + (portPath ~> cis)
        }
      }
      val srcName = Util.getName(ci.src.feature.get)
      val dstName = Util.getName(ci.dst.feature.get)

      add(srcName, F)
      add(dstName, T)
    }
    return T
  }

  def getMaxSequenceSize(components: ISZ[Component], types: AadlTypes): Z = {
    var max:Z = z"0"

    for(c <- components) {
      val numPorts = Util.getPorts(c, types, basePackage, z"0").size
      if(numPorts > max) {
        max = numPorts
      }
    }

    val aadlArraySize = Util.findMaxAadlArraySize(types)
    if(aadlArraySize > max) {
      max = aadlArraySize
    }

    return max
  }

}

object SeL4NixGen{
  def apply(dirs: ProjectDirectories, m: Aadl, o: Cli.ArsitOption, types: AadlTypes, previousPhase: Result) : ArsitResult =
    new SeL4NixGen(dirs, m, o, types, previousPhase).generator()
}

