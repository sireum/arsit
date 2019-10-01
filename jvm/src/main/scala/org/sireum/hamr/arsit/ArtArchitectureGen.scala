package org.sireum.hamr.arsit

import java.io.File

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.ops.ISZOps

import scala.language.implicitConversions

class ArtArchitectureGen(dir : File,
                         m: Aadl,
                         topPackageName: String,
                         arsitOptions: Cli.ArsitOption,
                         types: AadlTypes) {
  var componentId = 0
  var portId = 0
  var outDir : File = _

  var bridges : ISZ[ST] = ISZ()
  var components : ISZ[String] = ISZ[String]()
  var connections : ISZ[ST] = ISZ()

  var seenConnections: HashMap[Name, ISZ[Name]] = HashMap.empty

  var componentMap : HashMap[String, Component] = HashMap.empty

  var basePackage: String = _

  def generator() : (Z, Z) = {
    assert(dir.exists)
    basePackage = Util.sanitizeName(topPackageName)
    outDir = dir

    for(t <- types.typeMap.values) {
      emitType(t)
    }
    val baseTypes = StringTemplate.Base_Types(basePackage)
    Util.writeFile(new File(outDir, s"../data/${basePackage}/Base_Types.scala"), baseTypes, true)

    gen(m)

    (portId, componentId) // return the next available ids
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

    val arch = Template.architectureDescription(
      basePackage,
      architectureName,
      architectureDescriptionName,
      bridges,
      components,
      connections
    )

    Util.writeFile(new File(outDir, s"$basePackage/Arch.scala"), arch)

    val demo = Template.demo(basePackage, architectureName, architectureDescriptionName)
    Util.writeFile(new File(outDir, s"$basePackage/Demo.scala"), demo)
  }

  def getComponentId(component: Component): Z = {
    val id = componentId
    componentId += 1
    return id
  }

  def getPortId(): Z = {
    val id = portId
    portId += 1
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

    for(c <- m.subComponents){
      c.category match {
        case ComponentCategory.System | ComponentCategory.Process => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device =>
          if(Util.isThread(c) || arsitOptions.devicesAsThreads) {
            val name = Util.getName(c.identifier)
            bridges :+= genThread(c, name)
            components :+= name
          }
        case ComponentCategory.Subprogram => // not relevant for arch
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor =>
          Util.report(s"Skipping: ${c.category} component ${Util.getName(m.identifier)}", T)
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
      val varName = Util.getName(c.identifier)
      bridges :+= genThread(c, varName)
      components :+= varName
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${Util.getName(c.src.component)}.${Util.getLastName(c.src.feature.get)}",
        s"${Util.getName(c.dst.component)}.${Util.getLastName(c.dst.feature.get)}")
    }
  }

  def genThread(m:Component, varName: String) : ST = {
    assert(Util.isThread(m) || Util.isDevice(m))
    assert(m.connections.isEmpty)
    //assert(m.subComponents.isEmpty)

    val name: Names = Util.getNamesFromClassifier(m.classifier.get, basePackage)

    val id = getComponentId(m)

    val period: String = Util.getPeriod(m)

    val dispatchProtocol = Util.getSlangEmbeddedDispatchProtocol(m)

    val dispatchProtocolST: ST = {
      dispatchProtocol match {
        case DispatchProtocol.Sporadic => Template.sporadic(period)
        case DispatchProtocol.Periodic => Template.periodic(period)
      }
    }

    var ports: ISZ[ST] = ISZ()
    for (f <- Util.getFeatureEnds(m.features) if Util.isPort(f)) {
      val typ = Util.getFeatureEndType(f, types)
      ports :+= genPort(Port(f, m, typ, basePackage))
    }

    val bridgeTypeName: String =  s"${name.packageName}.${name.bridge}"

    return Template.bridge(varName, bridgeTypeName, id, dispatchProtocolST, ports)
  }

  def genPort(port: Port) : ST = {
    val id = getPortId()

    import FeatureCategory._
    val prefix = port.feature.category match {
      case EventPort | EventDataPort => "Event"
      case DataPort => "Data"
      case _ => throw new RuntimeException("Not handling " + port.feature.category)
    }

    import Direction._
    val mode = prefix + (port.feature.direction match {
      case In => "In"
      case Out => "Out"
      case _ => "???"
    })

    return Template.port(port.name, port.portType.qualifiedReferencedTypeName, id, port.path, mode)
  }

  def emitType(t: AadlType): Unit = {
    if(t.isInstanceOf[BaseType]){
      return
    }

    val typeNames: DataTypeNames = Util.getDataTypeNames(t, basePackage)

    var imports: org.sireum.Set[String] = org.sireum.Set.empty[String]

    val body: ST = t match {
      case e: EnumType => Template.enumType(typeNames, e.values)

      case e: RecordType =>
        var fldInits: ISZ[String] = ISZ()
        var flds: ISZ[ST] = ISZ()

        for(f <- e.fields.entries){
          val fname = f._1
          val fieldTypeNames = Util.getDataTypeNames(f._2, basePackage)

          fldInits = fldInits :+ fieldTypeNames.empty()

          flds = flds :+ st"${fname} : ${fieldTypeNames.qualifiedReferencedTypeName}"
        }

        Template.dataType(typeNames, flds, fldInits, None[ST]())

      case e: ArrayType =>
        val baseTypeNames = Util.getDataTypeNames(e.baseType, basePackage)
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
          Some(st"{ assert (value.size == ${dims(0)}) }")
        } else {
          None[ST]()
        }

        Template.dataType(typeNames, flds, ISZ(emptyInit), optChecks)

      case e: TODOType =>
        println(s"Don't know how to handle ${e}")
        Template.typeSkeleton(typeNames)

      case _ => halt(s"${t}")
    }

    val ts = Template.typeS(
      basePackage,
      typeNames.qualifiedPackageName,
      body,
      Template.payloadType(typeNames))

    Util.writeFile(new File(outDir, "../data/" + typeNames.filePath.toString), ts, true)
  }

  def allowConnection(c : ConnectionInstance, m : Component) : B = {
    val str = s"${Util.getName(c.name)}  from  ${Util.getName(m.identifier)}"

    if(c.src.component == c.dst.component){
      Util.report(s"Skipping: Port connected to itself. $str", T)
      return F
    }
    if(c.kind != ConnectionKind.Port){
      Util.report(s"Skipping: ${c.kind} connection.  $str", T)
      return F
    }

    val allowedComponents = Seq(ComponentCategory.Device, ComponentCategory.Thread)
    val catSrc = componentMap.get(Util.getName(c.src.component)).get.category
    val catDest = componentMap.get(Util.getName(c.dst.component)).get.category

    if(!allowedComponents.contains(catSrc) || !allowedComponents.contains(catDest)) {
      Util.report(s"Skipping: connection between ${catSrc} to ${catDest}.  $str", T)
      return F
    }

    if(seenConnections.contains(c.src.feature.get) && ISZOps(seenConnections.get(c.src.feature.get).get).contains(c.dst.feature.get)) {
      Util.report(s"Skipping: already handled connection: ${c.src.feature.get} to ${c.dst.feature.get}", T)
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

    @pure def sporadic (period: String) = st"""Sporadic(min = $period)"""
    @pure def periodic (period: String) = st"""Periodic(period = $period)"""

    @pure def port(name: String,
                   typ: String,
                   id: Z,
                   identifier: String,
                   mode: String): ST = {
      return st"""$name = Port[$typ] (id = $id, name = "$identifier", mode = $mode)"""
    }

    @pure def bridge(varName: String,
                     typeName: String,
                     id: Z,
                     dispatchProtocol: ST,
                     ports: ISZ[ST]) : ST = {
      return st"""val ${varName} : ${typeName} = ${typeName}(
                  |  id = $id,
                  |  name = "$varName",
                  |  dispatchProtocol = $dispatchProtocol,
                  |
                  |  ${(ports, ",\n")}
                  |)"""
    }

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

    @pure def architectureDescription(packageName: String,
                                      architectureName: String,
                                      architectureDescriptionName: String,
                                      bridges : ISZ[ST],
                                      components : ISZ[String],
                                      connections: ISZ[ST]
                                     ) : ST = {
      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import art._
                 |import art.PortMode._
                 |import art.DispatchPropertyProtocol._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |object $architectureName {
                 |  ${(bridges, "\n")}
                 |
                 |  val $architectureDescriptionName : ArchitectureDescription = {
                 |
                 |    ArchitectureDescription(
                 |      components = MSZ (${(components, ", ")}),
                 |
                 |      connections = ISZ (${(connections, ",\n")})
                 |    )
                 |  }
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
      return st"""@datatype class ${typeNames.typeName}() // type skeleton"""
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
                    payload: ST): ST = {

      return st"""// #Sireum
                 |
                 |package $packageName
                 |
                 |import org.sireum._
                 |import $topLevelPackageName._
                 |
                 |${Util.doNotEditComment()}
                 |
                 |$body
                 |$payload
                 |"""
    }
  }
}

object ArtArchitectureGen {
  def apply(dir: File, m: Aadl, topPackage: String, o: Cli.ArsitOption, types: AadlTypes) =
    new ArtArchitectureGen(dir, m, topPackage, o, types).generator()
}