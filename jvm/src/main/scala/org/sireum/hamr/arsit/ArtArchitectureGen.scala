package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.ops.ISZOps
import org.sireum.hamr.arsit.Util.reporter
import org.sireum.hamr.arsit.templates._

import scala.language.implicitConversions

class ArtArchitectureGen(directories: ProjectDirectories,
                         m: Aadl,
                         arsitOptions: Cli.ArsitOption,
                         types: AadlTypes) {
  var componentId = 0
  var portId: Z = 0

  val basePackage = arsitOptions.packageName
  var bridges : ISZ[ST] = ISZ()
  var components : ISZ[String] = ISZ[String]()
  var connections : ISZ[ST] = ISZ()

  var seenConnections: HashMap[Name, ISZ[Name]] = HashMap.empty

  var componentMap : HashMap[String, Component] = HashMap.empty

  var resources: ISZ[Resource] = ISZ()

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ SlangUtil.createResource(baseDir, paths, content, overwrite)
  }

  def generator() : PhaseResult = {
    for(t <- types.typeMap.values) {
      emitType(t)
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

    { // build the component map
      def r(c: Component): Unit = {
        assert(!componentMap.contains(Util.getName(c.identifier)))
        componentMap += (Util.getName(c.identifier) → c)
        for (s <- c.subComponents) r(s)
      }
      r(systems(0))
    }

    gen(systems(0))

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val arch = ArchTemplate.architectureDescription(
      basePackage,
      ISZ(),
      architectureName,
      architectureDescriptionName,
      bridges,
      components,
      connections
    )

    addResource(directories.architectureDir, ISZ(basePackage, "Arch.scala"), arch, T)

    val demo = Template.demo(basePackage, architectureName, architectureDescriptionName)
    addResource(directories.architectureDir, ISZ(basePackage, "Demo.scala"), demo, T)
  }

  def getComponentId(component: Component): Z = {
    val id = componentId
    componentId += 1
    return id
  }

  /*
  def getPortId(): Z = {
    val id = portId
    portId += 1
    return id
  }
  */
  
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

    for(c <- m.subComponents){
      c.category match {
        case ComponentCategory.System | ComponentCategory.Process => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device =>
          if(Util.isThread(c) || arsitOptions.devicesAsThreads) {
            val names = Util.getComponentNames(c, basePackage)
            bridges :+= genThread(c, names)
            components :+= names.instanceName
          }
        case ComponentCategory.Subprogram => // not relevant for arch
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor =>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.category} component ${Util.getName(m.identifier)}")
        case _ => throw new RuntimeException("Unexpected " + c)
      }
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${Util.getName(c.src.component)}.${Util.getLastName(c.src.feature.get)}",
        s"${Util.getName(c.dst.component)}.${Util.getLastName(c.dst.feature.get)}")
    }
  }

  def genThreadGroup(m: Component) : Unit = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents){
      assert(c.category == ComponentCategory.Thread)
      val names = Util.getComponentNames(c, basePackage)
      bridges :+= genThread(c, names)
      components :+= names.instanceName
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${Util.getName(c.src.component)}.${Util.getLastName(c.src.feature.get)}",
        s"${Util.getName(c.dst.component)}.${Util.getLastName(c.dst.feature.get)}")
    }
  }

  def genThread(m:Component, names: Names) : ST = {
    assert(Util.isThread(m) || Util.isDevice(m))
    assert(m.connections.isEmpty)
    
    val id = getComponentId(m)

    val period: Z = Util.getPeriod(m)

    val dispatchProtocol = Util.getSlangEmbeddedDispatchProtocol(m)

    val dispatchProtocolST: ST = ArchTemplate.dispatchProtocol(dispatchProtocol, period)

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m)

    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, portId)
    portId = portId + ports.size

    return ArchTemplate.bridge(names.instanceName, names.instanceName, names.bridgeTypeName, id, dispatchProtocolST, dispatchTriggers, ports)
  }

  def emitType(t: AadlType): Unit = {
    if(t.isInstanceOf[BaseType]){
      return
    }

    val typeNames: DataTypeNames = SlangUtil.getDataTypeNames(t, basePackage)

    var imports: org.sireum.Set[String] = org.sireum.Set.empty[String]

    var canOverwrite: B = T

    val body: ST = t match {
      case e: EnumType => Template.enumType(typeNames, e.values)

      case e: RecordType =>
        var fldInits: ISZ[String] = ISZ()
        var flds: ISZ[ST] = ISZ()

        for(f <- e.fields.entries){
          val fname = f._1
          val fieldTypeNames = SlangUtil.getDataTypeNames(f._2, basePackage)

          fldInits = fldInits :+ fieldTypeNames.empty()

          flds = flds :+ st"${fname} : ${fieldTypeNames.qualifiedReferencedTypeName}"
        }

        Template.dataType(typeNames, flds, fldInits, None[ST]())

      case e: ArrayType =>
        val baseTypeNames = SlangUtil.getDataTypeNames(e.baseType, basePackage)
        val baseTypeEmpty = baseTypeNames.empty()

        val dims = Util.getArrayDimensions(e)

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

        Template.dataType(typeNames, flds, ISZ(emptyInit), optChecks)

      case e: TODOType =>
        reporter.warn(None(), Util.toolName, s"Don't know how to handle ${e}")
        canOverwrite = F
        Template.typeSkeleton(typeNames)
        
      case _ => halt(s"${t}")
    }

    val ts = Template.typeS(
      basePackage,
      typeNames.qualifiedPackageName,
      body,
      Template.payloadType(typeNames),
      canOverwrite)

    addResource(directories.dataDir, ISZ(typeNames.filePath), ts, canOverwrite)
  }

  def allowConnection(c : ConnectionInstance, m : Component) : B = {
    val str = s"${Util.getName(c.name)}  from  ${Util.getName(m.identifier)}"

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
    
    val catSrc = componentMap.get(Util.getName(c.src.component)).get.category
    val catDest = componentMap.get(Util.getName(c.dst.component)).get.category

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

  object Template {

    @pure def connection(from: String, to: String) : ST = return st"""Connection(from = $from, to = $to)"""

    @pure def demo(packageName: String,
                   architectureName: String,
                   architectureDescriptionName: String) : ST = {
      return st"""${Util.doNotEditComment()}
                 |package $packageName
                 |
                 |object Demo extends App {
                 |  art.Art.run(${architectureName}.${architectureDescriptionName})
                 |}"""
    }

    @pure def enumType(typeNames: DataTypeNames,
                       values: ISZ[String]): ST = {
      val vals = values.map(m => st"'$m")
      return st"""@enum object ${typeNames.typeName} {
                 |  ${(vals, "\n")}
                 |}
                 |"""
    }

    @pure def dataType(typeNames: DataTypeNames,
                       fields: ISZ[ST],
                       paramInits: ISZ[String],
                       optChecks: Option[ST]): ST = {
      return st"""object ${typeNames.typeName} {
                 |  def empty(): ${typeNames.qualifiedTypeName} = {
                 |    return ${typeNames.qualifiedTypeName}(${(paramInits, ", ")})
                 |  }
                 |}
                 |
                 |@datatype class ${typeNames.typeName}(
                 |  ${(fields, ",\n")}) {
                 |  $optChecks
                 |}
                 |"""
    }

    @pure def typeSkeleton(typeNames: DataTypeNames): ST = {
      val typeName = typeNames.qualifiedReferencedTypeName
      val payloadTypeName = typeNames.payloadName
      val emptyPayload = typeNames.empty()

      return st"""object ${typeNames.typeName} {
                 |  def empty(): ${typeNames.qualifiedTypeName} = {
                 |    return ${typeNames.qualifiedTypeName}()
                 |  }
                 |}
                 |
                 |@datatype class ${typeNames.typeName}() // type skeleton
                 |"""
    }

    @pure def payloadType(typeNames: DataTypeNames) : ST = {
      val typeName = typeNames.qualifiedReferencedTypeName
      val payloadTypeName = typeNames.payloadName
      val emptyPayload = typeNames.empty()

      return st"""object $payloadTypeName {
                 |  def empty(): $payloadTypeName = {
                 |    return $payloadTypeName($emptyPayload)
                 |  }
                 |}
                 |
                 |@datatype class $payloadTypeName(value: $typeName) extends art.DataContent"""
    }

    @pure def typeS(topLevelPackageName: String,
                    packageName: String,
                    body: ST,
                    payload: ST,
                    canOverwrite: B): ST = {
      val overwrite = if(canOverwrite) {
        st"""
            |${Util.doNotEditComment() }
            |"""
      } else { st"" }

      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import $topLevelPackageName._
                 |$overwrite
                 |$body
                 |$payload
                 |"""
    }
  }
}

object ArtArchitectureGen {
  def apply(directories: ProjectDirectories, m: Aadl, o: Cli.ArsitOption, types: AadlTypes) =
    new ArtArchitectureGen(directories, m, o, types).generator()
}