// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.arsit.bts.BlessBehaviorProviderPlugin
import org.sireum.hamr.arsit.gcl.{GumboDatatypeProviderPlugin, GumboPlugin}
import org.sireum.hamr.arsit.templates.{EntryPointTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.message.Reporter

object ArsitPlugin {

  val defaultPlugins: ISZ[Plugin] = ISZ(
    BlessBehaviorProviderPlugin(),
    SingletonBridgeCodeProviderPlugin(),
    SingletonEntryPointProviderPlugin(),
    GumboDatatypeProviderPlugin(),
    GumboPlugin(),
    DefaultDatatypeProvider())

  @memoize def getDatatypeProviders(plugins: ISZ[Plugin]): ISZ[DatatypeProviderPlugin] = {
    return plugins.filter((p : Plugin) => p.isInstanceOf[DatatypeProviderPlugin]).map((m: Plugin) => m.asInstanceOf[DatatypeProviderPlugin])
  }

  @pure def getDatatypeProvider(plugins: ISZ[Plugin],
                                aadlType: AadlType,
                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): DatatypeProviderPlugin = {
    return getDatatypeProviders(plugins).filter((p: DatatypeProviderPlugin) =>
      p.canHandle(aadlType, resolvedAnnexSubclauses))(0)
  }


  @memoize def getBridgeCodeProviders(plugins: ISZ[Plugin]): BridgeCodeProviderPlugin = {
    val ret = plugins.filter((p: Plugin) => p.isInstanceOf[BridgeCodeProviderPlugin]).map((p: Plugin) => p.asInstanceOf[BridgeCodeProviderPlugin])
    if (ret.size != 1) {
      halt("Only the default bridge code provider is currently allowed")
    }
    return ret(0)
  }


  @memoize def getEntryPointProviders(plugins: ISZ[Plugin]): ISZ[EntryPointProviderPlugin] = {
    return plugins.filter((p: Plugin) => p.isInstanceOf[EntryPointProviderPlugin]).map((p: Plugin) => p.asInstanceOf[EntryPointProviderPlugin])
  }

  def getEntryPointProvider(plugins: ISZ[Plugin],
                            component: AadlThreadOrDevice,
                            resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): EntryPointProviderPlugin = {
    val ret = getEntryPointProviders(plugins).filter((ep: EntryPointProviderPlugin) => ep.canHandle(component, resolvedAnnexSubclauses))
    if (ret.isEmpty) {
      halt("infeasible")
    }
    // first ep provider who can handle component wins
    return ret(0)
  }


  @memoize def getBehaviorProviders(plugins: ISZ[Plugin]): ISZ[BehaviorProviderPlugin] = {
    return plugins.filter((p: Plugin) => p.isInstanceOf[BehaviorProviderPlugin]).map((p: Plugin) => p.asInstanceOf[BehaviorProviderPlugin])
  }

  def canHandleBehaviorProviders(plugins: ISZ[Plugin],
                                 component: AadlThreadOrDevice,
                                 resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = {
    for (bp <- getBehaviorProviders(plugins) if bp.canHandle(component, resolvedAnnexSubclauses)) {
      return T
    }
    return F
  }
}

@sig trait ArsitPlugin extends Plugin

@sig trait BehaviorProviderPlugin extends ArsitPlugin {

  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  // allows a plugin to provide its own behavior code implementation for component
  def handle(component: AadlThreadOrDevice,
             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
             suggestedFilename: String,
             componentDirectory: ISZ[String],

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,

             reporter: Reporter): ISZ[Resource]
}


@sig trait BehaviorEntryPointContributions

object BehaviorEntryPointPartialContributions {
  @strictpure def empty: BehaviorEntryPointPartialContributions =
    BehaviorEntryPointPartialContributions(ISZ(), ISZ(), ISZ(), ISZ(), None(), None(), ISZ(), ISZ(), ISZ(), ISZ())
}

object BehaviorEntryPointFullContributions {
  @strictpure def empty: BehaviorEntryPointFullContributions =
    BehaviorEntryPointFullContributions(ISZ(), ISZ(), ISZ(), ISZ(), st"", ISZ(), ISZ(), ISZ(), ISZ())
}

// allows plugin to fully provide the behavior code for a method.  An error will be thrown
// if there are more than one of these plugins for a pipeline
@datatype class BehaviorEntryPointFullContributions(val tags: ISZ[String],
                                                    val imports: ISZ[String],
                                                    val preObjectBlocks: ISZ[ST],

                                                    val preMethodBlocks: ISZ[ST],
                                                    val method: ST, // includes the method sig, optional contract, and body
                                                    val postMethodBlocks: ISZ[ST],

                                                    val postObjectBlocks: ISZ[ST],

                                                    val markers: ISZ[Marker],
                                                    val resources: ISZ[Resource]) extends BehaviorEntryPointContributions

// allows plugin to provide parts of the behavior code for a method that will be
// combined with those from other plugins in the same pipeline.
// ContractBlock types must be the same for all plugins (all return cases or general)
// an error will be thrown if more than one plugin provides an optBody
@datatype class BehaviorEntryPointPartialContributions(val tags: ISZ[String],
                                                       val imports: ISZ[String],
                                                       val preObjectBlocks: ISZ[ST],

                                                       val preMethodBlocks: ISZ[ST],

                                                       val contractBlock: Option[ContractBlock],

                                                       val optBody: Option[ST],

                                                       val postMethodBlocks: ISZ[ST],

                                                       val postObjectBlocks: ISZ[ST],

                                                       val markers: ISZ[Marker],
                                                       val resources: ISZ[Resource]) extends BehaviorEntryPointContributions

@sig trait ContractBlock

@datatype class CaseContractBlock(val cases: ISZ[ST]) extends ContractBlock

@datatype class NonCaseContractBlock(val contractReads: ISZ[ST],
                                     val contractRequires: ISZ[ST],
                                     val contractModifies: ISZ[ST],
                                     val contractEnsures: ISZ[ST],
                                     val contractFlows: ISZ[ST]) extends ContractBlock

@sig trait BehaviorEntryPointProviderPlugin extends ArsitPlugin {

  @pure def canHandle(entryPoint: EntryPoints.Type,
                      optInEventPort: Option[AadlPort],
                      component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  // allows a plugin to provide contributions to the generated code for
  // an entrypoint. BehaviorEntryPointProviderPlugins will not be called
  // for a component if a BehaviorProviderPlugin has already handled the
  // component
  def handle(entryPoint: EntryPoints.Type,
             optInEventPort: Option[AadlPort],
             component: AadlThreadOrDevice,
             excludeComponentImplementation: B,

             methodSignature: String, // e.g. def handlePortName(value: PortType, api: ApiType): Unit
             defaultMethodBody: ST,

             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             basePackageName: String,
             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,
             projectDirectories: ProjectDirectories,
             reporter: Reporter): BehaviorEntryPointContributions
}

@sig trait BridgeCodeProviderPlugin extends ArsitPlugin {
  def generate(nameProvider: NameProvider,
               component: AadlThreadOrDevice,
               ports: ISZ[Port],

               entryPointProvider: EntryPointProviderPlugin,

               symbolTable: SymbolTable,
               aadlTypes: AadlTypes,

               reporter: Reporter): BridgeCodeContributions
}

@sig trait EntryPointProviderPlugin extends ArsitPlugin {
  @pure def canHandle(component: AadlThreadOrDevice,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  def handle(component: AadlThreadOrDevice,
             nameProvider: NameProvider,
             ports: ISZ[Port],

             entryPointTemplate: EntryPointTemplate,

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,

             reporter: Reporter): EntryPointContributions
}

@datatype class DatatypeContribution(val datatype: Resource,
                                     val resources: ISZ[Resource])

@sig trait DatatypeProviderPlugin extends ArsitPlugin {
  @pure def canHandle(aadlType: AadlType,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  def handle(aadlType: AadlType,

             datatypeTemplate: IDatatypeTemplate,

             suggestFilename: String,
             dataDirectory: String,

             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,

             reporter: Reporter): DatatypeContribution
}


