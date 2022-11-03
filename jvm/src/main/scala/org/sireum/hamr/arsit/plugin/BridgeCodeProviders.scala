// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.arsit.templates.{ApiTemplate, StringTemplate}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil
import org.sireum.message.Reporter

@datatype class BridgeCodeContributions(bridge: ST,
                                        resources: ISZ[Resource])

@datatype class EntryPointContributions(imports: ISZ[String],
                                        bridgeCompanionBlocks: ISZ[String],
                                        entryPoint: ST,
                                        resources: ISZ[Resource])


@datatype class SingletonBridgeCodeProviderPlugin extends BridgeCodeProviderPlugin {
  @pure def name: String = {
    return "Singleton Bridge Code Provider Plugin"
  }

  def generate(nameProvider: NameUtil.NameProvider,
               component: AadlThreadOrDevice,
               ports: ISZ[Port],
               entryPointProvider: EntryPointProviderPlugin,
               symbolTable: SymbolTable,
               aadlTypes: AadlTypes,
               reporter: Reporter): BridgeCodeContributions = {

    val portParams: ISZ[String] = ports.map((p: Port) => {
      val artPortType: String = if (p.urgency.nonEmpty) "UrgentPort" else "Port"
      val portType = p.getPortTypeNames.qualifiedReferencedTypeName
      s"${p.name}: ${artPortType}[${portType}]"
    })

    val entryPointTemplate = SingletonEntryPointProviderPlugin.getEntryPointTemplate(nameProvider, component, ports)
    val entryPointContributions = entryPointProvider.handle(component, nameProvider, ports, entryPointTemplate, symbolTable, aadlTypes, reporter)

    val apiDecls: ISZ[ST] = ISZ(
      ApiTemplate.apiBridgeEntry(nameProvider, nameProvider.bridge, ports, T),
      ApiTemplate.apiBridgeEntry(nameProvider, nameProvider.bridge, ports, F))

    val companionObjectBlocks: Option[ST] = if (entryPointContributions.bridgeCompanionBlocks.isEmpty) None()
    else Some(st"${(entryPointContributions.bridgeCompanionBlocks, "\n\n")}")

    val imports: ISZ[ST] = ISZ(
      st"${nameProvider.packageName}.{${nameProvider.componentSingletonType} => component}"
    )

    val bridge: ST =
      st"""// #Sireum
          |
          |package ${nameProvider.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${nameProvider.basePackage}._
          |${addImports(imports ++ entryPointContributions.imports.map((m: String) => st"$m"))}
          |
          |${StringTemplate.doNotEditComment(None())}
          |
          |@datatype class ${nameProvider.bridge}(
          |  val id: Art.BridgeId,
          |  val name: String,
          |  val dispatchProtocol: DispatchPropertyProtocol,
          |  val dispatchTriggers: Option[ISZ[Art.PortId]],
          |
          |  ${(portParams, ",\n")}
          |  ) extends Bridge {
          |
          |  val ports : Bridge.Ports = Bridge.Ports(
          |    all = ISZ(${(ports.map((m: Port) => m.name), ",\n")}),
          |
          |    dataIns = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    dataOuts = ISZ(${(ports.filter((v: Port) => CommonUtil.isAadlDataPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventIns = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isInFeature(v.feature)).map((m: Port) => m.name), ",\n")}),
          |
          |    eventOuts = ISZ(${(ports.filter((v: Port) => CommonUtil.isEventPort(v.feature) && CommonUtil.isOutFeature(v.feature)).map((m: Port) => m.name), ",\n")})
          |  )
          |
          |  ${(apiDecls, "\n\n")}
          |
          |  val entryPoints : Bridge.EntryPoints =
          |    ${nameProvider.bridge}.EntryPoints(
          |      id,
          |
          |      ${(ports.map((p: Port) => s"${p.name}.id"), ",\n")},
          |
          |      dispatchTriggers,
          |
          |      ${(ISZ(ApiTemplate.apiInitializationId, ApiTemplate.apiOperationalId), ",\n")})
          |}
          |
          |object ${nameProvider.bridge} {
          |
          |  ${ApiTemplate.companionObjectApiInstances(nameProvider)}
          |  ${companionObjectBlocks}
          |
          |  ${entryPointContributions.entryPoint}
          |}"""

    return BridgeCodeContributions(bridge, entryPointContributions.resources)
  }

  def addImports(imports: ISZ[ST]): Option[ST] = {
    val s: Set[String] = Set.empty[String] ++ (imports.map((m: ST) => s"import ${m.render}"))
    return if (s.nonEmpty) Some(st"${(s.elements, "\n")}") else None()
  }
}