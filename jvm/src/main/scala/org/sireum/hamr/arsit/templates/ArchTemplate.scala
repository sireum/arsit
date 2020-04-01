// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.ir.{Direction, FeatureCategory}

object ArchTemplate {

  @pure def doNotEditComment(from: Option[String]): ST = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return st"// This file was auto-generated${_from}.  Do not edit"
  }
  
  @pure def dispatchProtocol(dp: DispatchProtocol.Type, period: Z): ST = {
    val ret: ST = dp match {
      case DispatchProtocol.Sporadic => st"Sporadic(min = $period)"
      case DispatchProtocol.Periodic =>st"Periodic(period = $period)"
    }  
    return ret
  }
  
  @pure def portType(name: String,
                 typ: String,
                 id: Z,
                 identifier: String,
                 mode: String,
                 urgency: Option[Z]): ST = {
    val artPortType: String = if(urgency.nonEmpty) "UrgentPort" else "Port"
    val _urgency: String = if(urgency.nonEmpty) s", urgency = ${urgency.get}" else ""
    return st"""val $name = ${artPortType}[$typ] (id = $id, name = "$identifier", mode = $mode${_urgency})"""
  }
  
  def genPort(port: Port) : ST = {
    val id = port.portId

    import FeatureCategory._
    val prefix: String = port.feature.category match {
      case EventPort => "Event"
      case EventDataPort => "Event"
      case DataPort => "Data"
      case _ => halt(s"Not handling ${port.feature.category}")
    }

    val dir: String = port.feature.direction match {
      case Direction.In => "In"
      case Direction.Out => "Out"
      case _ => "???"
    }
    
    val mode: String = s"${prefix}${dir}" 

    return portType(port.name, port.portType.qualifiedReferencedTypeName, id, port.path, mode, port.urgency)
  }
    
  @pure def bridge(bridgeIdentifier: String,
                   instanceName: String,
                   typeName: String,
                   id: Z,
                   dispatchProtocol: ST,
                   dispatchTriggers: Option[ISZ[String]],
                   ports: ISZ[Port]): ST = {
    val _ports = ports.map(p => genPort(p))
    val _args = ports.map(p => st"${p.name} = ${p.name}")

    val _dispatchTriggers: ST = if(dispatchTriggers.isEmpty) st"None()" else st"Some(ISZ(${(dispatchTriggers.get.map(f => s"${f}.id"), ", ")}))"
    
    return st"""val ${bridgeIdentifier} : ${typeName} = {
               |  ${(_ports, "\n")}
               |  
               |  ${typeName}(
               |    id = $id,
               |    name = "$instanceName",
               |    dispatchProtocol = $dispatchProtocol,
               |    dispatchTriggers = ${_dispatchTriggers},
               |    
               |    ${(_args, ",\n")}
               |  )
               |}"""
  }
  
  @pure def architectureDescription(packageName: String,
                                    imports: ISZ[String],
                                    architectureName: String,
                                    architectureDescriptionName: String,
                                    bridges : ISZ[ST],
                                    components : ISZ[String],
                                    connections: ISZ[ST]): ST = {
    val _imports = imports.map(m => st"import ${m}")
    
    return st"""// #Sireum
               |
               |package $packageName
               |
               |import org.sireum._
               |import art._
               |import art.PortMode._
               |import art.DispatchPropertyProtocol._
               |${(_imports, "\n")}
               |
               |${doNotEditComment(None())}
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
