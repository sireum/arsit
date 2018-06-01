package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.util.MList
import org.sireum.util.MMap

import scala.language.implicitConversions

class ArtArchitectureGen {
  var componentId = 0
  var portId = 0
  var outDir : File = null

  var bridges : ISZ[ST] = ISZ()
  var components : ISZ[String] = ISZ[String]()
  var connections : ISZ[ST] = ISZ()

  val seenConnections: MMap[Name, MList[Name]] = org.sireum.util.mmapEmpty

  val componentMap : MMap[String, Component] = org.sireum.util.mmapEmpty

  var topLevelPackageName: String = _

  def generator(dir: File, m: Aadl, topPackageName: String) : Unit = {
    assert(dir.exists)
    topLevelPackageName = Util.sanitizeName(topPackageName)

    outDir = dir

    {
      def r(c: Component): Unit = {
        assert(!componentMap.contains(Util.getName(c.identifier)))
        componentMap += (Util.getName(c.identifier) → c)
        for (s <- c.subComponents) r(s)
      }
      for (c <- m.components) r(c)
    }

    for(c <- m.components)
      gen(c)

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val arch = Template.architectureDescription(
      topLevelPackageName,
      architectureName,
      architectureDescriptionName,
      bridges,
      components,
      connections
    )

    Util.writeFile(new File(outDir, topLevelPackageName + "/Arch.scala"), arch)

    val demo = Template.demo(topLevelPackageName, architectureName, architectureDescriptionName)
    Util.writeFile(new File(outDir, topLevelPackageName + "/Demo.scala"), demo)
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
          val name = Util.getName(c.identifier)
          bridges :+= genThread(c, name)
          components :+= name
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor =>
          println(s"Skipping: ${c.category} component ${Util.getName(m.identifier)}")
        case _ => throw new RuntimeException("Unexpected " + c)
      }
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${Util.getName(c.src.component)}.${Util.getLastName(c.src.feature)}",
        s"${Util.getName(c.dst.component)}.${Util.getLastName(c.dst.feature)}")
    }
  }

  def genThreadGroup(m: Component) : Unit = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents){
      assert(c.category == ComponentCategory.Thread)
      val name = Util.getName(c.identifier)
      bridges :+= genThread(c, name)
      components :+= name
    }

    for(c <- m.connectionInstances if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${Util.getName(c.src.component)}.${Util.getLastName(c.src.feature)}",
        s"${Util.getName(c.dst.component)}.${Util.getLastName(c.dst.feature)}")
    }
  }

  def genThread(m:Component, varName: String) : ST = {
    assert(m.category == ComponentCategory.Thread || m.category == ComponentCategory.Device)
    assert(m.connections.isEmpty)
    assert(m.subComponents.isEmpty)

    val name: Names = Util.getNamesFromClassifier(m.classifier.get, topLevelPackageName)
    val pathName = Util.getName(m.identifier)

    val id = getComponentId(m)

    val period: ST = {
      Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Period) match {
        case Some(x) =>
          assert(x.unit.get == org.sireum.String("ps"))
          // convert picoseconds to milliseconds.  x.value was a double in osate
          // ps, ns => ps * 1000, us => ns * 1000, ms => us * 1000
          val v = x.value.toString.toDouble / 1e9
          st"""${v.toLong}"""
        case _ => st"""1"""
      }
    }

    val dispatchProtocol: ST = {
      Util.getDiscreetPropertyValue[ValueProp](m.properties, Util.DispatchProtocol) match {
        case Some(x) =>
          x.value.toString match {
            case "Aperiodic" | "Sporadic" => Template.sporadic(period)
            case "Periodic" | "Hybrid" => Template.periodic(period)
          }
        case _ =>
          if (m.category == ComponentCategory.Device) Template.periodic(period)
          else ???
      }
    }

    var ports: ISZ[ST] = ISZ()
    for (f <- m.features if Util.isPort(f))
      ports :+= genPort(f)

    return Template.bridge(varName, s"${name.packageName}.${name.bridge}", pathName, id, dispatchProtocol, ports)
  }

  def genPort(p:Feature) : ST = {
    val name = Util.getLastName(p.identifier)
    val typ: String = p.classifier match {
      case Some(c) => Util.cleanName(c.name) + (if(Util.isEnum(p.properties)) ".Type" else "")
      case _ => Util.EmptyType
    }
    val id = getPortId()
    val identifier = s"${Util.getName(p.identifier)}"

    import FeatureCategory._
    val prefix = p.category match {
      case EventPort | EventDataPort => "Event"
      case DataPort => "Data"
      case _ => throw new RuntimeException("Not handling " + p.category)
    }

    import Direction._
    val mode = prefix + (p.direction match {
      case In => "In"
      case Out => "Out"
      case _ => "???" //throw new RuntimeException(s"Not handling ${p.direction} for $name")
    })

    return Template.port(name, typ, id, identifier, mode)
  }

  def allowConnection(c : ConnectionInstance, m : Component) : B = {
    val str = s"${Util.getName(c.name)}  from  ${Util.getName(m.identifier)}"

    if(c.src.component == c.dst.component){
      println(s"Skipping: Port connected to itself. $str")
      return F
    }
    if(c.kind != ConnectionKind.Port){
      println(s"Skipping: ${c.kind} connection.  $str")
      return F
    }

    val allowedComponents = Seq(ComponentCategory.Device, ComponentCategory.Thread)
    val catSrc = componentMap(Util.getName(c.src.component)).category
    val catDest = componentMap(Util.getName(c.dst.component)).category

    if(!allowedComponents.contains(catSrc) || !allowedComponents.contains(catDest)) {
      println(s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return F
    }

    if(seenConnections.contains(c.src.feature) && seenConnections(c.src.feature).contains(c.dst.feature)) {
      println(s"Skipping: already handled connection: ${c.src.feature} to ${c.dst.feature}")
      return F
    }
    seenConnections.getOrElseUpdate(c.src.feature, org.sireum.util.mlistEmpty[Name]) += c.dst.feature

    return T
  }

  object Template {

    @pure def sporadic (period: ST) = st"""Sporadic(min = $period)"""
    @pure def periodic (period: ST) = st"""Periodic(period = $period)"""

    @pure def port(name: String,
                   typ: String,
                   id: Z,
                   identifier: String,
                   mode: String): ST = {
      return st"""$name = Port[$typ] (id = $id, name = "$identifier", mode = $mode)"""
    }

    @pure def bridge(varName: String,
                     typeName: String,
                     pathName: String,
                     id: Z,
                     dispatchProtocol: ST,
                     ports: ISZ[ST]) : ST = {
      return st"""val ${varName} : ${typeName} = ${typeName}(
                  |  id = $id,
                  |  name = "$pathName",
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
                 |package $topLevelPackageName
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
  }
}

object ArtArchitectureGen {
  def apply(dir: File, m: Aadl, topPackage: String) = new ArtArchitectureGen().generator(dir, m, topPackage)
}