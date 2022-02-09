// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.codegen.common.symbols.{AadlFeature, AadlPort}
import org.sireum.hamr.codegen.common.{CommonUtil, Names}
import org.sireum.hamr.ir._

object ApiTemplate {

  val apiOperationalId: String = "operational_api"
  val apiOperationalBridgeId: String = "c_operational_api"

  val apiInitializationId: String = "initialization_api"
  val apiInitializationBridgeId: String = "c_initialization_api"

  def api(packageName: String,
          basePackageName: String,
          names: Names,
          ports: ISZ[Port],
          integrationContracts: Map[AadlPort, (ST, ST)]): ST = {

    val portDefs: ISZ[ST] = st"id: Art.BridgeId" +:
      ops.ISZOps(ports).map((p: Port) => st"${p.name}_Id : Art.PortId")

    val portTraitDefs: ISZ[ST] = ops.ISZOps(portDefs).map((s: ST) => st"def ${s}")
    val portParams: ISZ[ST] = ops.ISZOps(portDefs).map((s: ST) => st"val ${s}")

    val inPorts = ports.filter((p: Port) => CommonUtil.isInPort(p.feature))
    val outPorts = ports.filter((p: Port) => !CommonUtil.isInPort(p.feature))

    def getContract(f: AadlFeature): Option[(ST, ST)] = {
      val ret: Option[(ST,ST)] = f match {
        case i: AadlPort => integrationContracts.get(i)
        case _ => None()
      }
      return ret
    }

    val getters = inPorts.map((p: Port) => getterApi(p, getContract(p.aadlFeature)))

    val setters = outPorts.map((p: Port) => setterApi(p, getContract(p.aadlFeature)))

    val ret: ST =
      st"""// #Sireum
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import art._
          |import ${basePackageName}._
          |
          |@sig trait ${names.api} {
          |  ${(portTraitDefs, "\n")}
          |
          |  ${(setters, "\n\n")}
          |
          |  def logInfo(msg: String): Unit = {
          |    Art.logInfo(id, msg)
          |  }
          |
          |  def logDebug(msg: String): Unit = {
          |    Art.logDebug(id, msg)
          |  }
          |
          |  def logError(msg: String): Unit = {
          |    Art.logError(id, msg)
          |  }
          |}
          |
          |@datatype class ${names.apiInitialization} (
          |  ${(portParams, ",\n")}) extends ${names.api}
          |
          |@datatype class ${names.apiOperational} (
          |  ${(portParams, ",\n")}) extends ${names.api} {
          |
          |  ${(getters, "\n\n")}
          |}
          |"""

    return ret
  }

  def entryPointParams(names: Names): ISZ[ST] = {
    var ret : ISZ[ST] = ISZ(
      st"${apiInitializationId}: ${names.apiInitialization}",
      st"${apiOperationalId}: ${names.apiOperational}")
    return ret
  }

  def apiBridgeEntry(names: Names,
                     bridgeCompanionObjectName: String,
                     ports: ISZ[Port],
                     isEntry: B): ST = {
    val (id, typ, companionId): (String, String, String) = {
      if(isEntry) {
        (apiInitializationId, names.apiInitialization, apiInitializationBridgeId)
      } else {
        (apiOperationalId, names.apiOperational, apiOperationalBridgeId)
      }
    }

    val _ports = ops.ISZOps(ports).map((p: Port) => s"${p.name}.id")
    val ret: ST = {
      st"""val ${id} : ${typ} = {
          |  val api = ${typ}(
          |    id,
          |    ${(_ports, ",\n")}
          |  )
          |  ${bridgeCompanionObjectName}.${companionId} = Some(api)
          |  api
          |}"""
    }
    return ret
  }

  def companionObjectApiInstances(names: Names): ST = {
    val ret: ST =
      st"""var ${apiInitializationBridgeId}: Option[${names.apiInitialization}] = None()
          |var ${apiOperationalBridgeId}: Option[${names.apiOperational}] = None()"""
    return ret
  }

  def addId(s: String): String = { return s"${s}_Id" }

  @pure def putValue(p: Port): ST = {
    val q = p.getPortTypeNames.qualifiedPayloadName
    val isEmpty = p.getPortTypeNames.isEmptyType()
    return st"""Art.putValue(${addId(p.name)}, ${q}${if (isEmpty) "()" else "(value)"})"""
  }

  def setterApi(p: Port, integrationContracts: Option[(ST, ST)]): ST = {
    val q = p.getPortTypeNames.qualifiedReferencedTypeName
    val isEmpty = p.getPortTypeNames.isEmptyType()

    val (integrationMethods, integrationContract): (Option[ST], Option[ST]) = integrationContracts match {
      case Some((s1, s2)) => (Some(s1), Some(s2))
      case _ => (None(),None())
    }

    val ret: ST = p.feature.category match {
      case FeatureCategory.DataPort =>
        st"""${integrationMethods}
            |def put_${p.name}(value : ${q}) : Unit = {
            |  ${integrationContract}
            |  ${putValue(p)}
            |}"""
      case FeatureCategory.EventPort =>
        st"""${integrationMethods}
            |def put_${p.name}(${if (isEmpty) "" else s"value : ${q}"}) : Unit = {
            |  ${integrationContract}
            |  ${putValue(p)}
            |}"""
      case FeatureCategory.EventDataPort =>
        st"""${integrationMethods}
            |def put_${p.name}(${if (isEmpty) "" else s"value : ${q}"}) : Unit = {
            |  ${integrationContract}
            |  ${putValue(p)}
            |}"""
      case _ => halt("Unexpected: $p")
    }
    return ret
  }

  @pure def getterApi(p: Port, integrationContracts: Option[(ST, ST)]): ST = {
    val isEvent = CommonUtil.isAadlEventPort(p.feature)
    val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
    val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
    val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
    val value: String = if (isEvent) "Empty()" else "v"

    val (integrationMethods, integrationContract): (Option[ST], Option[ST]) = integrationContracts match {
      case Some((s1, s2)) => (Some(s1), Some(s2))
      case _ => (None(),None())
    }

    val ret: ST =
      st"""${integrationMethods}
          |def get_${p.name}() : Option[${typeName}] = {
          |  ${integrationContract}
          |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) =>
          |      Art.logError(id, s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |      None[${typeName}]()
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}"""
    return ret
  }
}
