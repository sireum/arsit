package org.sireum.aadl.arsit

import java.io.File

import org.sireum._
import org.sireum.util.MMap
import org.sireum.Some

import org.sireum.aadl.skema.ast._

import scala.collection.immutable.{Set => mSet}
import scala.language.implicitConversions

object ArtArchitectureGen {

  var componentId = 0
  var portId = 0
  var outDir : File = null
  var imports : mSet[aString] = mSet()

  var bridges : ISZ[(String, ST)] = ISZ()
  var components : ISZ[String] = ISZ[String]()
  var connections : ISZ[ST] = ISZ()

  var componentMap : MMap[String, Component] = org.sireum.util.mmapEmpty

  type sString = scala.Predef.String
  type aString = org.sireum.String

  implicit def sireumString2ST(s:org.sireum.String) : ST = st"""$s"""
  implicit def string2ST(s:scala.Predef.String) : ST = st"""$s"""
  implicit def string2SireumString(s:scala.Predef.String) : org.sireum.String = org.sireum.String(s)

  def generator(dir: File, m: AadlXml) : Unit = {
    assert(dir.exists)

    outDir = dir

    {
      def r(c: Component): Unit = {
        assert(!componentMap.contains(c.identifier.get))
        componentMap += (c.identifier.get -> c)
        for (s <- c.subComponents) r(s)
      }
      for (c <- m.components) r(c)
    }

    for(c <- m.components)
      gen(c)

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val arch = Template.architectureDescription(
      ISZ(imports.toSeq:_*),
      architectureName,
      architectureDescriptionName,
      bridges,
      components,
      connections
    )

    Util.writeFile(new File(outDir, "Arch.scala"), arch.render.toString)

    val demo = Template.demo(architectureName, architectureDescriptionName)
    Util.writeFile(new File(outDir, "Demo.scala"), demo.render.toString)
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

  def genContainer(m: Component) : ST = {
    assert (m.category == ComponentCategory.System || m.category == ComponentCategory.Process)

    for(c <- m.subComponents){
      c.category match {
        case ComponentCategory.System | ComponentCategory.Process => genContainer(c)
        case ComponentCategory.ThreadGroup => genThreadGroup(c)
        case ComponentCategory.Thread | ComponentCategory.Device =>
          val name = c.identifier match {
            case Some(x) => x
            case _ => org.sireum.String("")
          }
          bridges :+= (name, genThread(c))
          components :+= name
        case ComponentCategory.Bus | ComponentCategory.Memory | ComponentCategory.Processor =>
          println(s"Skipping: ${c.category} component ${m.identifier.get}")
        case _ => throw new RuntimeException("Unexpected " + c)
      }
    }

    for(c <- m.connections if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${c.src.component}.${c.src.feature}",
        s"${c.dst.component}.${c.dst.feature}")
    }

    return st""" """
  }

  def genThreadGroup(m: Component) : ST = {
    assert(m.category == ComponentCategory.ThreadGroup)

    for(c <- m.subComponents){
      assert(c.category == ComponentCategory.Thread)
      val name = c.identifier match {
        case Some(x) => x
        case _ => org.sireum.String("")
      }
      bridges :+= (name, genThread(c))
      components :+= name
    }

    for(c <- m.connections if allowConnection(c, m)) {
      connections :+= Template.connection(
        s"${c.src.component}.${c.src.feature}",
        s"${c.dst.component}.${c.dst.feature}")
    }

    return st""" """
  }

  def genThread(m:Component) : ST = {
    assert(m.category == ComponentCategory.Thread || m.category == ComponentCategory.Device)
    assert(m.connections.isEmpty)
    assert(m.subComponents.isEmpty)

    if(m.classifier.nonEmpty)
      imports += s"import " +
        m.classifier.get.name.toString.split("::").toSeq.dropRight(1).mkString(".") + "._"

    val name: org.sireum.String = m.identifier match {
      case Some(x) => Util.getBridgeName(x)
      case _ => ""
    }

    val id = getComponentId(m)

    val period: ST = {
      Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Period) match {
        case Some(x) => x.value
        case _ => "???"
      }
    }

    val dispatchProtocol: ST = {
      Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.DispatchProtocol) match {
        case Some(x) =>
          x.value.toString match {
            case "Aperiodic" | "Sporadic" => Template.sporadic(period)
            case "Periodic" | "Hybrid" => Template.periodic(period)
          }
        case _ =>
          if (m.category == ComponentCategory.Device) Template.periodic("1")
          else "???"
      }
    }

    var ports: ISZ[ST] = ISZ()
    for (f <- m.features if Util.isPort(f))
      ports :+= genPort(f, name)

    return Template.bridge(name, id, dispatchProtocol, ports)
  }

  def genPort(p:Feature, componentId: String) : ST = {
    val name = p.identifier
    val typ = p.classifier match {
      case Some(c) => Util.cleanName(c.name) + (if(Util.isEnum(p.properties)) ".Type" else "")
      case _ => Util.EmptyType
    }
    val id = getPortId()
    val identifier = s"$componentId.${p.identifier}"

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

  def allowConnection(c : Connection, m : Component) : B = {
    //val str = s"${c.src.component}.${c.src.feature} --> ${c.dst.component}.${c.dst.feature}  from  ${m.identifier.get}"
    val str = s"${c.name.get}  from  ${m.identifier.get}"

    if(c.src.component == c.dst.component){
      println(s"Skipping: Port connected to itself. $str")
      return false
    }
    if(c.kind != ConnectionKind.Port){
      println(s"Skipping: ${c.kind} connection.  $str")
      return false
    }

    val allowedComponents = Seq(ComponentCategory.Device, ComponentCategory.Thread)
    val catSrc = componentMap(c.src.component).category
    val catDest = componentMap(c.dst.component).category
    if(!allowedComponents.contains(catSrc) || !allowedComponents.contains(catDest)) {
      println(s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return false
    }

    return true
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

    @pure def bridge(name: String,
                     id: Z,
                     dispatchProtocol: ST,
                     ports: ISZ[ST]) : ST = {
      return st"""${name}(
                  |  id = $id,
                  |  name = "$name",
                  |  dispatchProtocol = $dispatchProtocol,
                  |
                  |  ${(ports, ",\n")}
                  |)"""
    }

    @pure def connection(from: String, to: String) : ST = return st"""Connection(from = $from, to = $to)"""

    @pure def demo(architectureName: String,
                   architectureDescriptionName: String) : ST = {
      return st"""object Demo extends App {
                 |  art.Art.run(${architectureName}.${architectureDescriptionName})
                 |}"""
    }

    @pure def architectureDescription(imports : ISZ[String],
                                      architectureName: String,
                                      architectureDescriptionName: String,
                                      bridges : ISZ[(String, ST)],
                                      components : ISZ[String],
                                      connections: ISZ[ST]
                                     ) : ST = {
      return st"""// #Sireum
                 |
                 |import org.sireum._
                 |import art._
                 |import art.PortMode._
                 |import art.DispatchPropertyProtocol._
                 |${(imports, "\n")}
                 |
                 |object $architectureName {
                 |  val $architectureDescriptionName : ArchitectureDescription = {
                 |
                 |    ${ var s = ""
                         for(b <- bridges)
                           s += "val " + b._1 + " = " + b._2.render + "\n"
                         s
                      }
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
