package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.ops.ISZOps
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates._
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._

import scala.language.implicitConversions

class ArchitectureGenerator(directories: ProjectDirectories,
                            m: Aadl,
                            arsitOptions: Cli.ArsitOption,
                            symbolTable: SymbolTable,
                            types: AadlTypes) {
  var componentId = 0
  var portId: Z = 0

  val basePackage = arsitOptions.packageName
  var bridges : ISZ[ST] = ISZ()
  var components : ISZ[String] = ISZ[String]()
  var connections : ISZ[ST] = ISZ()

  var seenConnections: HashMap[Name, ISZ[Name]] = HashMap.empty

  var resources: ISZ[Resource] = ISZ()

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ SlangUtil.createResource(baseDir, paths, content, overwrite)
  }

  def generator() : PhaseResult = {
    if(!types.rawConnections) {
      for (t <- types.typeMap.values) {
        emitType(t)
      }
    }

    val baseTypes = StringTemplate.Base_Types(basePackage)
    addResource(directories.dataDir, ISZ(basePackage, "Base_Types.scala"), baseTypes, T)

    gen(m)

    return PhaseResult(resources, portId, componentId)
  }

  def gen(m: Aadl): Unit = {
    val systems = m.components.filter(c => c.category == ComponentCategory.System)
    assert(systems.size == 1)

    m.components.filter(c => c.category != ComponentCategory.System).foreach(c => assert(c.category == ComponentCategory.Data))

    gen(systems(0))

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val arch = ArchitectureTemplate.architectureDescription(
      basePackage,
      ISZ(),
      architectureName,
      architectureDescriptionName,
      bridges,
      components,
      connections
    )

    addResource(directories.architectureDir, ISZ(basePackage, "Arch.scala"), arch, T)

    val demo = ArchitectureTemplate.demo(basePackage, architectureName, architectureDescriptionName)
    addResource(directories.architectureDir, ISZ(basePackage, "Demo.scala"), demo, T)
  }

  def getComponentId(component: Component): Z = {
    val id = componentId
    componentId += 1
    return id
  }

  def gen(c: Component) : Unit = {
    c.category match {
      case ComponentCategory.System | ComponentCategory.Process => genContainer(c)
      case ComponentCategory.ThreadGroup => genThreadGroup(c)
      case _ =>
        for(_c <- c.subComponents)
          gen(_c)
    }
  }

  def genContainer(m: Component) : Unit = {
    assert (m.category == ComponentCategory.System || m.category == ComponentCategory.Process)

    for(c <- m.subComponents) {
      c.category match {
        case ComponentCategory.System | ComponentCategory.Process => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device =>
          if(CommonUtil.isThread(c) || arsitOptions.devicesAsThreads) {
            val names = Names(c, basePackage)
            bridges :+= genThread(c, names)
            components :+= names.instanceName
          }
        case ComponentCategory.Subprogram => // not relevant for arch
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor | ComponentCategory.VirtualProcessor=>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.category} component ${CommonUtil.getName(m.identifier)}")
        case _ => throw new RuntimeException("Unexpected " + c)
      }
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= ArchitectureTemplate.connection(
        s"${CommonUtil.getName(c.src.component)}.${CommonUtil.getLastName(c.src.feature.get)}",
        s"${CommonUtil.getName(c.dst.component)}.${CommonUtil.getLastName(c.dst.feature.get)}")
    }
  }

  def genThreadGroup(m: Component) : Unit = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents){
      assert(c.category == ComponentCategory.Thread)
      val names = Names(c, basePackage)
      bridges :+= genThread(c, names)
      components :+= names.instanceName
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= ArchitectureTemplate.connection(
        s"${CommonUtil.getName(c.src.component)}.${CommonUtil.getLastName(c.src.feature.get)}",
        s"${CommonUtil.getName(c.dst.component)}.${CommonUtil.getLastName(c.dst.feature.get)}")
    }
  }

  def genThread(m:Component, names: Names) : ST = {
    assert(CommonUtil.isThread(m) || CommonUtil.isDevice(m))
    assert(m.connections.isEmpty)

    val id = getComponentId(m)

    val period: Z = Util.getPeriod(m)

    val dispatchProtocol = PropertyUtil.getDispatchProtocol(m) match {
      case Some(x) => x
      case _ =>
      if(CommonUtil.isDevice(m)) {
        Dispatch_Protocol.Periodic
      } else {
        halt("HAMR codegen only supports Periodic or Sporadic threads")
      }
    }

    val dispatchProtocolST: ST = ArchitectureTemplate.dispatchProtocol(dispatchProtocol, period)

    val dispatchTriggers: Option[ISZ[String]] = SlangUtil.getDispatchTriggers(m)

    val ports: ISZ[Port] = SlangUtil.getPorts(m, types, basePackage, portId)
    portId = portId + ports.size

    val _ports = ports.map((p : Port) => ArchitectureTemplate.genPort(p))
    val _portArgs = ports.map((p : Port) => st"${p.name} = ${p.name}")

    return ArchitectureTemplate.bridge(
      names.instanceName,
      names.instanceName,
      names.bridgeTypeName,
      id,
      dispatchProtocolST,
      dispatchTriggers,
      _ports,
      _portArgs)
  }

  def emitType(t: AadlType): Unit = {
    if(t.isInstanceOf[BaseType]){
      return
    }

    val typeNames: DataTypeNames = SlangUtil.getDataTypeNames(t, basePackage)

    var imports: org.sireum.Set[String] = org.sireum.Set.empty[String]

    var canOverwrite: B = T

    val body: ST = t match {
      case e: EnumType => TypeTemplate.enumType(typeNames, e.values)

      case e: RecordType =>
        var fldInits: ISZ[String] = ISZ()
        var flds: ISZ[ST] = ISZ()

        for(f <- e.fields.entries){
          val fname = f._1
          val fieldTypeNames = SlangUtil.getDataTypeNames(f._2, basePackage)

          fldInits = fldInits :+ fieldTypeNames.empty()

          flds = flds :+ st"${fname} : ${fieldTypeNames.qualifiedReferencedTypeName}"
        }

        TypeTemplate.dataType(typeNames, flds, fldInits, None[ST]())

      case e: ArrayType =>
        val baseTypeNames = SlangUtil.getDataTypeNames(e.baseType, basePackage)
        val baseTypeEmpty = baseTypeNames.empty()

        val dims = TypeUtil.getArrayDimensions(e)

        val emptyInit: String = if(dims.nonEmpty){
          assert(dims.size == 1)
          s"ISZ.create(${dims(0)}, ${baseTypeEmpty})"
        } else {
          s"ISZ(${baseTypeEmpty})"
        }

        val flds = ISZ(st"value : ISZ[${baseTypeNames.qualifiedReferencedTypeName}]")

        val optChecks: Option[ST] = if(dims.nonEmpty) {
          Some(st"//{  assert (value.size == ${dims(0)}) }")
        } else {
          None[ST]()
        }

        TypeTemplate.dataType(typeNames, flds, ISZ(emptyInit), optChecks)

      case e: TODOType =>
        reporter.warn(None(), Util.toolName, s"Don't know how to handle ${e}")
        canOverwrite = F
        TypeTemplate.typeSkeleton(typeNames)
        
      case _ => halt(s"${t}")
    }

    val ts = TypeTemplate.typeS(
      basePackage,
      typeNames.qualifiedPackageName,
      body,
      TypeTemplate.payloadType(typeNames),
      canOverwrite)

    addResource(directories.dataDir, ISZ(typeNames.filePath), ts, canOverwrite)
  }

  def allowConnection(c : ConnectionInstance, m : Component) : B = {
    val str = s"${CommonUtil.getName(c.name)}  from  ${CommonUtil.getName(m.identifier)}"

    if(c.src.component == c.dst.component){
      reporter.info(None(), Util.toolName, s"Skipping: Port connected to itself. $str")
      return F
    }
    if(c.kind != ConnectionKind.Port){
      reporter.info(None(), Util.toolName, s"Skipping: ${c.kind} connection.  $str")
      return F
    }

    val allowedComponents = {
      if(arsitOptions.devicesAsThreads) Seq(ComponentCategory.Device, ComponentCategory.Thread)
      else Seq(ComponentCategory.Thread)
    }
    
    val catSrc = symbolTable.airComponentMap.get(CommonUtil.getName(c.src.component)).get.category
    val catDest = symbolTable.airComponentMap.get(CommonUtil.getName(c.dst.component)).get.category

    if(!allowedComponents.contains(catSrc) || !allowedComponents.contains(catDest)) {
      reporter.info(None(), Util.toolName, s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return F
    }

    if(seenConnections.contains(c.src.feature.get) && ISZOps(seenConnections.get(c.src.feature.get).get).contains(c.dst.feature.get)) {
      reporter.info(None(), Util.toolName, s"Skipping: already handled connection: ${c.src.feature.get} to ${c.dst.feature.get}")
      return F
    }

    val seq = if(!seenConnections.contains(c.src.feature.get)) {
      ISZ(c.dst.feature.get)
    } else {
      seenConnections.get(c.src.feature.get).get :+ c.dst.feature.get
    }
    seenConnections = seenConnections + (c.src.feature.get ~> seq)

    return T
  }
}

object ArchitectureGenerator {
  def apply(directories: ProjectDirectories, m: Aadl, o: Cli.ArsitOption, symbolTable: SymbolTable, types: AadlTypes) =
    new ArchitectureGenerator(directories, m, o, symbolTable, types).generator()
}