// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.Util.nameProvider
import org.sireum.hamr.arsit.nix.NixGen
import org.sireum.hamr.arsit.plugin.{ArsitPlugin, DefaultDatatypeProvider}
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.arsit.util.{ArsitOptions, SchedulerUtil}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.{ExperimentalOptions, ResourceUtil}
import org.sireum.hamr.ir
import org.sireum.hamr.ir.ConnectionInstance
import org.sireum.ops.ISZOps

@record class ArchitectureGenerator(val directories: ProjectDirectories,
                                    val rootSystem: AadlSystem,
                                    val arsitOptions: ArsitOptions,
                                    val symbolTable: SymbolTable,
                                    val types: AadlTypes,
                                    val plugins: MSZ[Plugin]) {
  var componentId: Z = 0
  var portId: Z = 0

  val basePackage: String = arsitOptions.packageName
  var bridges: ISZ[ST] = ISZ()
  var components: ISZ[String] = ISZ[String]()
  var connections: ISZ[ST] = ISZ()

  var seenConnections: HashMap[ir.Name, ISZ[ir.Name]] = HashMap.empty

  var resources: ISZ[FileResource] = ISZ()

  def generate(): Result = {
    if (!types.rawConnections) {
      // TODO allow for customizations of base types
      for (aadlType <- types.typeMap.values if !aadlType.isInstanceOf[BaseType]) {
        val defaultTemplate = DefaultDatatypeProvider.genDefaultTemplate(aadlType)

        val aadlComponent = symbolTable.componentMap.get(ISZ(aadlType.name)).get
        val resolvedAnnexSubclauses: ISZ[AnnexClauseInfo] = symbolTable.annexClauseInfos.get(aadlComponent) match {
          case Some(annexInfos) => annexInfos
          case _ => ISZ()
        }

        val dpProvider = ArsitPlugin.getDatatypeProvider(plugins, aadlType, resolvedAnnexSubclauses)
        val dpContributions = dpProvider.handle(aadlType, defaultTemplate, aadlType.nameProvider.filePath, directories.dataDir,
          resolvedAnnexSubclauses, symbolTable, types, reporter)
        resources = (resources :+ dpContributions.datatype) ++ dpContributions.resources
      }
    }

    val baseTypes = TypeTemplate.Base_Types(basePackage)
    resources = resources :+ ResourceUtil.createResourceH(Util.pathAppend(directories.dataDir, ISZ(basePackage, "Base_Types.scala")), baseTypes, T, T)

    generateInternal()

    return ArsitResult(
      resources = resources,
      auxResources = ISZ(),
      maxPort = portId,
      maxComponent = componentId,
      maxConnection = connections.size
    )
  }

  def generateInternal(): Unit = {
    gen(rootSystem)

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val touchMethod: Option[ST] =
      if (NixGen.willTranspile(arsitOptions.platform)) {
        val components: ISZ[AadlThreadOrDevice] =
          if (arsitOptions.devicesAsThreads) symbolTable.getThreadOrDevices()
          else symbolTable.getThreads().map(m => m.asInstanceOf[AadlThreadOrDevice])

        val typeTouches = NixGen.genTypeTouches(types)
        val apiTouches = NixGen.genApiTouches(types, basePackage, components)
        val scheduleTouches = SchedulerUtil.getSchedulerTouches(symbolTable, arsitOptions.devicesAsThreads)
        Some(SeL4NixTemplate.genTouchMethod(typeTouches, apiTouches, scheduleTouches))
      }
      else {
        None()
      }

    val arch = ArchitectureTemplate.architectureDescription(
      packageName = basePackage,
      imports = ISZ(),
      architectureName = architectureName,
      architectureDescriptionName = architectureDescriptionName,
      bridges = bridges,
      components = components,
      connections = connections,
      touchMethod = touchMethod
    )

    addResource(directories.architectureDir, ISZ(basePackage, "Arch.scala"), arch, T)

    val demo = ArchitectureTemplate.demo(basePackage, architectureName, architectureDescriptionName)
    addResource(directories.architectureDir, ISZ(basePackage, "Demo.scala"), demo, F)

    val schedulers = SchedulerTemplate.schedulers(basePackage, components,
      SchedulerUtil.getProcessorTimingProperties(symbolTable),
      SchedulerUtil.getThreadTimingProperties(symbolTable, arsitOptions.devicesAsThreads),
      SchedulerUtil.getFramePeriod(symbolTable))
    addResource(directories.architectureDir, ISZ(basePackage, "Schedulers.scala"), schedulers, T)

    val scheduleProvider = SchedulerTemplate.scheduleProvider(basePackage)
    addResource(directories.architectureDir, ISZ(basePackage, "ScheduleProvider.scala"), scheduleProvider, T)

    val inspectorDemo = InspectorTemplate.inspectorDemo(basePackage)
    addResource(directories.inspectorDir, ISZ(basePackage, "InspectorDemo.scala"), inspectorDemo, T)
  }

  def getComponentId(): Z = {
    val id = componentId
    componentId = componentId + 1
    return id
  }

  def gen(c: AadlComponent): Unit = {
    c match {
      case s: AadlSystem => genContainer(s)
      case s: AadlProcess => genContainer(s)

      case s: AadlThreadGroup => genThreadGroup(s)

      case _ =>
        for (_c <- c.subComponents) {
          gen(_c)
        }
    }
  }

  def genContainer(m: AadlComponent): Unit = {
    assert(m.isInstanceOf[AadlSystem] || m.isInstanceOf[AadlProcess])

    for (c <- m.subComponents) {
      c match {
        case s: AadlSystem => genContainer(s)
        case s: AadlProcess => genContainer(s)

        case s: AadlThreadGroup => genThreadGroup(s)

        case s: AadlThread =>
          val names = nameProvider(s.component, basePackage)
          bridges = bridges :+ genThread(s, names)
          components = components :+ names.instanceName

        case s: AadlDevice =>
          if (arsitOptions.devicesAsThreads) {
            val names = nameProvider(s.component, basePackage)
            bridges = bridges :+ genThread(s, names)
            components = components :+ names.instanceName
          }

        case s: AadlSubprogram => // not relevant for arch

        case x =>
          reporter.info(None(), Util.toolName, s"Skipping: ${x.component.category} component ${CommonUtil.getName(x.component.identifier)}")
      }
    }

    connections = connections ++ m.connectionInstances
      .filter((ci: ir.ConnectionInstance) => allowConnection(ci, m.component))
      .map((ci: ir.ConnectionInstance) => processConnectionInstance(ci))
  }


  def genThreadGroup(m: AadlThreadGroup): Unit = {

    for (c <- m.subComponents) {
      assert(c.isInstanceOf[AadlThread])
      val names = nameProvider(c.component, basePackage)
      bridges = bridges :+ genThread(c.asInstanceOf[AadlThread], names)
      components = components :+ names.instanceName
    }

    connections = connections ++ m.connectionInstances
      .filter((ci: ir.ConnectionInstance) => allowConnection(ci, m.component))
      .map((ci: ir.ConnectionInstance) => processConnectionInstance(ci))
  }

  def genThread(m: AadlThreadOrDevice, names: NameProvider): ST = {
    assert(!m.isInstanceOf[AadlDevice] || arsitOptions.devicesAsThreads)

    assert(m.connectionInstances.isEmpty)

    val id = getComponentId()

    val period: Z = CommonUtil.getPeriod(m)

    val dispatchProtocol: Dispatch_Protocol.Type = m.dispatchProtocol

    val dispatchProtocolST: ST = ArchitectureTemplate.dispatchProtocol(dispatchProtocol, period)

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m.component)

    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, portId)
    portId = portId + ports.size

    val _ports = ports.map((p: Port) => ArchitectureTemplate.genPort(p))
    val _portArgs = ports.map((p: Port) => st"${p.name} = ${p.name}")

    return ArchitectureTemplate.bridge(
      bridgeIdentifier = names.instanceName,
      instanceName = names.instanceName,
      typeName = names.bridgeTypeName,
      id = id,
      dispatchProtocol = dispatchProtocolST,
      dispatchTriggers = dispatchTriggers,
      ports = _ports,
      portArguments = _portArgs)
  }

  def processConnectionInstance(ci: ConnectionInstance): ST = {
    val srcComponentId = CommonUtil.getName(ci.src.component)
    val srcFeatureId = ci.src.feature.get.name
    val srcComponentFeatureId = symbolTable.featureMap.get(srcFeatureId).get.identifier

    val dstComponentId = CommonUtil.getName(ci.dst.component)
    val dstFeatureId = ci.dst.feature.get.name
    val dstComponentFeatureId = symbolTable.featureMap.get(dstFeatureId).get.identifier

    return ArchitectureTemplate.connection(
      s"${srcComponentId}.${srcComponentFeatureId}",
      s"${dstComponentId}.${dstComponentFeatureId}")
  }

  def allowConnection(c: ConnectionInstance, srcComponent: ir.Component): B = {
    val str = s"${CommonUtil.getName(c.name)}  from  ${CommonUtil.getName(srcComponent.identifier)}"

    if (c.src.component == c.dst.component) {
      reporter.info(None(), Util.toolName, s"Skipping: Port connected to itself. $str")
      return F
    }
    if (c.kind != ir.ConnectionKind.Port) {
      reporter.info(None(), Util.toolName, s"Skipping: ${c.kind} connection.  $str")
      return F
    }

    val allowedComponents: ISZ[ir.ComponentCategory.Type] = {
      if (arsitOptions.devicesAsThreads) ISZ(ir.ComponentCategory.Device, ir.ComponentCategory.Thread)
      else ISZ(ir.ComponentCategory.Thread)
    }

    val catSrc = symbolTable.airComponentMap.get(c.src.component.name).get.category
    val catDest = symbolTable.airComponentMap.get(c.dst.component.name).get.category

    if (!ISZOps(allowedComponents).contains(catSrc) || !ISZOps(allowedComponents).contains(catDest)) {
      reporter.info(None(), Util.toolName, s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return F
    }

    if (seenConnections.contains(c.src.feature.get) && ISZOps(seenConnections.get(c.src.feature.get).get).contains(c.dst.feature.get)) {
      reporter.info(None(), Util.toolName, s"Skipping: already handled connection: ${c.src.feature.get} to ${c.dst.feature.get}")
      return F
    }

    val seq: ISZ[ir.Name] =
      if (!seenConnections.contains(c.src.feature.get)) ISZ(c.dst.feature.get)
      else seenConnections.get(c.src.feature.get).get :+ c.dst.feature.get

    seenConnections = seenConnections + (c.src.feature.get ~> seq)

    return T
  }

  def addExeResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createExeResource(Util.pathAppend(baseDir, paths), content, overwrite)
  }

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(baseDir, paths), content, overwrite)
  }
}
