package org.sireum.aadl

import org.sireum._
import org.sireum.Some
import org.sireum.ops._
import org.sireum.ops.ISZOps._
import org.sireum.aadl.ast.{ComponentCategory, FeatureCategory, JSON}

object ArchitectureGen {

  var componentId = 0
  var portId = 0

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

  def gen(m: ast.AadlTop) : ST = {
    return m match {
      case x: ast.Component =>
        x.category match {
          case ComponentCategory.ThreadGroup =>
            genThreadGroup(m.asInstanceOf[ast.Component])
          case cat =>
            throw new RuntimeException(s"Not handling: $cat")
        }
      case _ => throw new RuntimeException(s"Not handling: $m")
    }

  }

  def genThreadGroup(m: ast.Component) : ST = {
    var imports :ISZ[String] = ISZ()

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
      "packageName",
      imports,
      "arch",
      "ad",
      bridges,
      components,
      connections
    )

    return x
  }

  def genPort(p:ast.Feature) : ST = {
    val name = p.identifier
    val typ = p.classifier match {
      case Some(name) => name.toString.replace("::", ".")
      case _ => "Empty"
    }
    val id = getPortId()
    val identifier = p.identifier

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

  def genThread(m:ast.Component) : ST = {
    assert(m.category == ComponentCategory.Thread)
    assert(m.connections.isEmpty)
    assert(m.subComponents.isEmpty)

    val name = m.identifier match {
      case Some(x) => x
      case _ => org.sireum.String("")
    }

    val id = getComponentId(m)
    val dispatchProtocol = st""" ???? """

    var ports: ISZ[ST] = ISZ()
    import FeatureCategory._
    for (f <- m.features) {
      f.category match{
        case DataPort | EventDataPort | EventPort =>
          ports :+= genPort(f)
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

  object Template {

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
      return st"""${name}Bridge(
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
