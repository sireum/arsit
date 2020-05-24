// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.codegen.common.symbols.Dispatch_Protocol
import org.sireum.hamr.ir.{Direction, FeatureCategory}

object ArchTemplate {

  @pure def doNotEditComment(from: Option[String]): ST = {
    val _from: String = if (from.nonEmpty) s" from ${from.get}" else ""
    return st"// This file was auto-generated${_from}.  Do not edit"
  }
  
  @pure def dispatchProtocol(dp: Dispatch_Protocol.Type, period: Z): ST = {
    val ret: ST = dp match {
      case Dispatch_Protocol.Sporadic => st"Sporadic(min = $period)"
      case Dispatch_Protocol.Periodic => st"Periodic(period = $period)"
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
      case FeatureCategory.EventPort => "Event"
      case FeatureCategory.EventDataPort => "Event"
      case FeatureCategory.DataPort => "Data"
      case _ => halt(s"Not handling ${port.feature.category}")
    }

    val dir: String = port.feature.direction match {
      case Direction.In => "In"
      case Direction.Out => "Out"
      case _ => "???"
    }
    
    val mode: String = s"${prefix}${dir}" 

    return portType(
      port.name,
      port.getPortTypeNames.qualifiedReferencedTypeName,
      id,
      port.path,
      mode,
      port.urgency)
  }
    
  @pure def bridge(bridgeIdentifier: String,
                   instanceName: String,
                   typeName: String,
                   id: Z,
                   dispatchProtocol: ST,
                   dispatchTriggers: Option[ISZ[String]],
                   ports: ISZ[ST],
                   portArguments: ISZ[ST]
                  ): ST = {
    val _dispatchTriggers: ST =
      if(dispatchTriggers.isEmpty) st"None()" 
      else st"Some(ISZ(${(dispatchTriggers.get.map((f: String) => s"${f}.id"), ", ")}))"
    
    return st"""val ${bridgeIdentifier} : ${typeName} = {
               |  ${(ports, "\n")}
               |  
               |  ${typeName}(
               |    id = $id,
               |    name = "$instanceName",
               |    dispatchProtocol = $dispatchProtocol,
               |    dispatchTriggers = ${_dispatchTriggers},
               |    
               |    ${(portArguments, ",\n")}
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
    val _imports = imports.map((m: String) => st"import ${m}")
    
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
