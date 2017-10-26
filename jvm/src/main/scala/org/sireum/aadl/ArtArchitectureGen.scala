package org.sireum.aadl

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.Some
import org.sireum.ops._
import org.sireum.ops.ISZOps._
import org.sireum.aadl.ast.{ComponentCategory, FeatureCategory, JSON}
import scala.collection.immutable.{Set => mSet}
import scala.language.implicitConversions

object ArtArchitectureGen {

  var componentId = 0
  var portId = 0
  var outDir : File = null
  var rootPackage : String = ""
  var imports : mSet[aString] = mSet()

  type sString = scala.Predef.String
  type aString = org.sireum.String

  implicit def sireumString2ST(s:org.sireum.String) : ST = st"""$s"""
  implicit def string2ST(s:scala.Predef.String) : ST = st"""$s"""
  implicit def string2SireumString(s:scala.Predef.String) : org.sireum.String = org.sireum.String(s)

  def generator(dir: File, m: ast.AadlXml) : Unit = {
    outDir = dir
    if(!outDir.exists && !outDir.mkdirs()){
      println(s"Error occured while trying to mkdirs on ${dir.getAbsolutePath}")
      return
    }
    rootPackage = outDir.getName

    gen(m)
  }

  def getComponentId(component: ast.Component): Z = {
    val id = componentId
    componentId += 1
    return id
  }

  def getPortId(): Z = {
    val id = portId
    portId += 1
    return id
  }

  def gen(m: ast.AadlXml) : Unit = {
    var arch: ST = st""" """"

    for(c <- m.components) {
      c.category match {
        case ComponentCategory.ThreadGroup =>
          arch = genThreadGroup(c)
        case cat =>
          throw new RuntimeException(s"Not handling: $cat")
      }
    }
  }

  def genThreadGroup(m: ast.Component) : ST = {
    var bridges : ISZ[(String, ST)] = ISZ()
    for(c <- m.subComponents){
      assert(c.category == ComponentCategory.Thread)
      val name = c.identifier match {
        case Some(x) => x
        case _ => org.sireum.String("")
      }
      bridges :+= (name, genThread(c))
    }
    val components = bridges.map(x => x._1)

    var connections : ISZ[ST] = ISZ()
    for(c <- m.connections) {
      connections :+= Template.connection(
        s"${c.src.component}.${c.src.feature}",
        s"${c.dst.component}.${c.dst.feature}")
    }

    val x = Template.architectureDescription(
      rootPackage,
      ISZ(imports.toSeq:_*),
      "Arch",
      "ad",
      bridges,
      components,
      connections
    )

    Util.writeFile(new File(outDir, "Arch.scala"), x.render.toString)

    return x
  }

  def genThread(m:ast.Component) : ST = {
    assert(m.category == ComponentCategory.Thread)
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
      Util.getDiscreetPropertyValue[ast.UnitProp](m.properties, Util.Period) match {
        case Some(x) => x.value
        case _ => "???"
      }
    }

    val dispatchProtocol: ST = {
      Util.getDiscreetPropertyValue[ast.UnitProp](m.properties, Util.DispatchProtocol) match {
        case Some(x) =>
          x.value.toString match {
            case "Aperiodic" | "Sporadic" => Template.sporadic(period)
            case "Periodic" | "Hybrid" => Template.periodic(period)
          }
        case _ => "???"
      }
    }

    var ports: ISZ[ST] = ISZ()
    import FeatureCategory._
    for (f <- m.features) {
      f.category match{
        case DataPort | EventDataPort | EventPort =>
          ports :+= genPort(f, name)
        case _ =>
      }
    }

    return Template.bridge(
      name,
      id,
      dispatchProtocol,
      ports
    )
  }

  def genPort(p:ast.Feature, componentId: String) : ST = {
    val name = p.identifier
    val typ = p.classifier match {
      case Some(c) =>
        //imports += s"import ${Util.getPackageName(rootPackage, c.name)}"
        Util.cleanName(c.name) + (if(Util.isEnum(p.properties)) ".Type" else "")
      case _ => "Empty"
    }
    val id = getPortId()
    val identifier = s"$componentId.${p.identifier}"

    import ast.FeatureCategory._
    val prefix = p.category match {
      case EventPort => "Event"
      case DataPort | EventDataPort => "Data"
      case _ => throw new RuntimeException("Not handling " + p.category)
    }

    import ast.Direction._
    val mode = prefix + (p.direction match {
      case In => "In"
      case Out => "Out"
      case _ => throw new RuntimeException("Not handling " + p.direction)
    })

    return Template.port(name, typ, id, identifier, mode)
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

    @pure def connection(from: String, to: String) : ST = {
      return st"""Connection(from = $from, to = $to)"""
    }

    @pure def architectureDescription(packageName:String,
                                      imports : ISZ[String],
                                      architectureName: String,
                                      architectureDescriptionName: String,
                                      bridges : ISZ[(String, ST)],
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
