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

  var maxStackSize: Z = -1

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

    val components = componentMap.entries.filter(p =>
      Util.isThread(p._2) || (Util.isDevice(p._2) && arsitOptions.devicesAsThreads))

    val rootCOutputDir: Os.Path = if(arsitOptions.outputCDir.nonEmpty) {
      Os.path(arsitOptions.outputCDir.get)
    } else {
      Os.path(dirs.srcDir) / "c/sel4"
    }
    
    var transpilerScripts: Map[String, (ST, CTranspilerOption)] = Map.empty
    var sel4CompileScripts: ISZ[ST] = ISZ()
    
    for ((archVarName, m) <- components) {

      val names: Names = Util.getComponentNames(m, basePackage)

      val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, z"0")
        
      val dispatchStatus = genDispatchStatus(ports, Util.getSlangEmbeddedDispatchProtocol(m))

      val instanceName: String = names.instanceName
      
      val globals: ST = genGlobals(ports, instanceName)

      val receiveInput: ST = genReceiveInput(ports, instanceName)

      val putValue: ST = genPutValue(ports, instanceName)

      val getValue: ST = genGetValue(ports, instanceName)

      val sendOutput: ST = genSendOutput(ports, instanceName)
      
      val extensionObject: ST = genExtensionObject(ports, instanceName)
      
      val typeTouches: ST = genTypeTouches(types)
      
      val main = Sel4NixTemplate.main(
        basePackage,
        instanceName,
        dispatchStatus,
        globals,
        receiveInput,
        getValue,
        putValue,
        sendOutput,
        extensionObject,
        typeTouches)

      addResource(
        dirs.seL4NixDir,
        ISZ(basePackage, instanceName, s"${instanceName}.scala"),
        main,
        T)

      val extensionObjectStub: ST = genExtensionObjectStub(ports, basePackage, instanceName)
      
      addResource(
        dirs.seL4NixDir,
        ISZ(basePackage, instanceName, s"${getExtObjectStubName(instanceName)}.scala"),
        extensionObjectStub,
        T)
      
      val period: Z = Util.getPeriod(m)
      val dispatchProtocol: DispatchProtocol.Type = Util.getDispatchProtocol(m).get
      val imports: ISZ[String] = ISZ(s"${basePackage}._")
      
      val bridge = ArchTemplate.bridge(
        instanceName,
        names.bridgeTypeName,
        z"0",
        ArchTemplate.dispatchProtocol(dispatchProtocol, period),
        Util.getDispatchTriggers(m),
        ports)

      val packageName = s"${basePackage}.${instanceName}"
      
      val ad = ArchTemplate.architectureDescription(
        packageName,
        imports,
        Sel4NixTemplate.isolatedArchitectureName,
        "ad",
        ISZ(bridge),
        ISZ(instanceName),
        ISZ()
      )

      addResource(
        dirs.seL4NixDir,
        ISZ(basePackage, instanceName, s"${Sel4NixTemplate.isolatedArchitectureName}.scala"),
        ad,
        T)

      val cOutputDir: Os.Path = rootCOutputDir / instanceName
      
      val relPath = s"${cOutputDir.up.name}/${instanceName}" 
      sel4CompileScripts = sel4CompileScripts :+ TranspilerTemplate.compileLib(relPath)

      val trans = genTranspiler(basePackage, instanceName, ports.size, cOutputDir)
      transpilerScripts = transpilerScripts + (instanceName ~> trans)
    }
    
    val sel4CompileScript = TranspilerTemplate.compileLibPreamble(sel4CompileScripts)
    addExeResource(rootCOutputDir.value, ISZ("..", "compile-hamr-lib.sh"), sel4CompileScript, T)
    
    val scripts = transpilerScripts.values.map(m => m._1)
    transpilerOptions = transpilerOptions ++ transpilerScripts.values.map(m => m._2)
    
    val transpileScript = TranspilerTemplate.transpilerScriptPreamble(scripts)

    addExeResource(dirs.binDir, ISZ("transpile-sel4.sh"), transpileScript, T)

    addResource(cExtensionDir, ISZ("ext.c"), SlangUtil.getLibraryFile("ext.c"), F)
    addResource(cExtensionDir, ISZ("ext.h"), SlangUtil.getLibraryFile("ext.h"), F)
  }
  
  def genGlobals(ports: ISZ[Port],
                 instanceName: String): ST = {
    val _ports: ISZ[ST] = ports.map(m =>
      Sel4NixTemplate.portVariable(instanceName, m.name, m.nameId)
    )
    return st"${(_ports, "\n\n")}"
  }

  def genDispatchStatus(ports: ISZ[Port],
                        value: DispatchProtocol.Type): ST = {
    val body: ST = value match {
      case DispatchProtocol.Periodic => st"return TimeTriggered()"
      case DispatchProtocol.Sporadic => {
        val inEventPorts = ports.filter(p => Util.isInFeature(p.feature) && Util.isEventPort(p.feature))
        val checks: ISZ[ST] = inEventPorts.map(p =>{
          st"""if(${p.name}.nonEmpty) {
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
                      instanceName: String): ST = {
    val inPorts: ISZ[Port] = ports.filter(p => Util.isInFeature(p.feature))
    val entries: ISZ[ST] = inPorts.map(p => 
      st"${p.name} = SeL4_${instanceName}.receive_${p.name}()"
    )
    
    return Sel4NixTemplate.receiveInput(st"${(entries, "\n\n")}")
  }
  
  def genPutValue(ports: ISZ[Port],
                  instanceName: String): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => Util.isOutFeature(p.feature))
    
    val options: ISZ[(ST, ST)] = outPorts.map(p => {
      val test: ST = st"portId == ${p.nameId}" 
      val assign: ST = st"${p.name} = Some(data)"
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
      val assign: ST = st"return ${p.name}"
      (test, assign)
    })
    val optEls: ST = st"""halt(s"Unexpected: ${instanceName}.getValue called with: $${portId}")"""
    val ifelses = Sel4NixTemplate.ifEsleHelper(options, Some(optEls))

    return Sel4NixTemplate.getValue(ifelses)
  }
  
  def genSendOutput(ports: ISZ[Port],
                    instanceName: String): ST = {
    val outPorts: ISZ[Port] = ports.filter(p => Util.isOutFeature(p.feature))
    val entries: ISZ[ST] = outPorts.map(p => {
      st"""if(${p.name}.nonEmpty) {
          |  SeL4_${instanceName}.send_${p.name}(${p.name}.get)
          |  ${p.name} = noData
          |}"""
    })
    return Sel4NixTemplate.sendOutput(st"${(entries, "\n\n")}")
  }

  def genExtensionObject(ports: ISZ[Port], instanceName: String): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if(Util.isInFeature(p.feature)) {
        st"def receive_${p.name}(): Option[DataContent] = $$"
      } else {
        st"def send_${p.name}(d: DataContent): Unit = $$"
      }
    })
    val name = getExtObjectName(instanceName)
    return Sel4NixTemplate.extensionObject(name, st"${(entries, "\n\n")}")
  }
  
  def genExtensionObjectStub(ports: ISZ[Port], packageName: String, instanceName: String): ST = {
    val entries: ISZ[ST] = ports.map(p => {
      if(Util.isInFeature(p.feature)) {
        st"""def receive_${p.name}(): Option[DataContent] = halt("stub")"""
      } else {
        st"""def send_${p.name}(d: DataContent): Unit = halt("stub")"""
      }
    })
    val name = getExtObjectStubName(instanceName)
    return Sel4NixTemplate.extensionObjectStub(
      packageName, instanceName, name, st"${(entries, "\n\n")}")
  }

  def getExtObjectName(instanceName: String): String = {
    return s"SeL4_${instanceName}"
  }

  def getExtObjectStubName(instanceName: String): String = {
    return s"${getExtObjectName(instanceName)}_Ext"
  }

  def genTranspiler(basePackage: String,
                    instanceName: String,
                    numPorts: Z,
                    cOutputDir: Os.Path): (ST, CTranspilerOption) = {

    val packageName = s"${basePackage}/${instanceName}"
    val appName = s"${basePackage}.${instanceName}.${instanceName}" 
      
    val components = componentMap.entries.filter(p =>
      Util.isThread(p._2) || (Util.isDevice(p._2) && arsitOptions.devicesAsThreads))

    var sourcePaths: ISZ[String] = ISZ(
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("art")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("bridge")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("component")),
      SlangUtil.pathAppend(dirs.srcMainDir, ISZ("data")),
      SlangUtil.pathAppend(dirs.seL4NixDir, ISZ(packageName))
    )
    
    val excludes:ISZ[String] = if(arsitOptions.excludeImpl) {
      for ((archVarName, m) <- components) yield {
        val name: Names = Util.getComponentNames(m, basePackage)
        s"${name.packageName}.${name.componentImpl}"
      }
    } else {
      ISZ()
    }

    var extensions: ISZ[String] = ISZ(s"${cExtensionDir}/ext.c", s"${cExtensionDir}/ext.h")
    var buildApps: B = F
    var additionalInstructions: Option[ST] = Some(st"""FILE=$${OUTPUT_DIR}/CMakeLists.txt
                                                      |echo -e "\n\nadd_definitions(-DCAMKES)" >> $$FILE""")

    val maxArraySize: Z = ops.ISZOps(ISZ(arsitOptions.maxArraySize, numPorts, previousPhase.maxComponent)).foldLeft((a: Z, b: Z) => if(a > b) a else b, z"0")
    
    val customSequenceSizes: ISZ[String] = ISZ(
      s"MS[Z,art.Bridge]=1",
      s"MS[Z,MOption[art.Bridge]]=1",
      s"IS[Z,art.UPort]=${numPorts}",
      s"IS[Z,art.UConnection]=1" // no connetions, but arg has to be > 0

      // not valid
      //s"MS[org.sireum.Z,org.sireum.Option[art.UPort]]=${maxPortsForComponents}"
    )

    val customConstants: ISZ[String] = ISZ(
      s"art.Art.maxComponents=1",
      s"art.Art.maxPorts=${numPorts}"
    )

    val apps: ISZ[String] = ISZ(appName)
    val forwards: ISZ[String] = ISZ(s"art.ArtNative=${appName}")
    
    return TranspilerTemplate.transpiler(
      sourcePaths,
      cOutputDir,
      dirs.binDir,
      apps,
      forwards,
      arsitOptions.bitWidth,
      maxArraySize,
      arsitOptions.maxStringSize,
      customSequenceSizes,
      customConstants,
      maxStackSize,
      extensions,
      excludes,
      buildApps,
      additionalInstructions
    )
  }


  def genTypeTouches(types: AadlTypes): ST = {
    var a: ISZ[ST] = ISZ()
    var counter: Z = z"0"
    for(t <- types.typeMap.entries) {
      val typ = t._2
      val typeNames: DataTypeNames = Util.getDataTypeNames(typ, basePackage)
      a = a :+ st"val t${counter} = ${typeNames.empty()}"
      counter = counter + z"1"
    }
    return st"${(a, "\n")}"
  }
}

object SeL4NixGen{
  def apply(dirs: ProjectDirectories, m: Aadl, o: Cli.ArsitOption, types: AadlTypes, previousPhase: Result) : ArsitResult =
    new SeL4NixGen(dirs, m, o, types, previousPhase).generator()
}

