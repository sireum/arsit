// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.bts.BlessBehaviorProviderPlugin
import org.sireum.hamr.arsit.gcl.{GumboDatatypeProviderPlugin, GumboPlugin, GumboXPlugin}
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.{BehaviorEntryPointContributions, ObjectContributions}
import org.sireum.hamr.arsit.templates.{EntryPointTemplate, IDatatypeTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, Port, ProjectDirectories}
import org.sireum.hamr.codegen.common.containers.{Marker, FileResource}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.message.Reporter

object ArsitPlugin {

  @strictpure def defaultPlugins: MSZ[Plugin] = MSZ(
    BlessBehaviorProviderPlugin(),
    SingletonBridgeCodeProviderPlugin(),
    SingletonEntryPointProviderPlugin(),
    GumboDatatypeProviderPlugin(),
    GumboPlugin(),
    GumboXPlugin(),
    DefaultDatatypeProvider())

  @memoize def getDatatypeProviders(plugins: MSZ[Plugin]): MSZ[DatatypeProviderPlugin] = {
    return plugins.filter((p: Plugin) => p.isInstanceOf[DatatypeProviderPlugin]).map((m: Plugin) => m.asInstanceOf[DatatypeProviderPlugin])
  }

  @pure def getDatatypeProvider(plugins: MSZ[Plugin],
                                aadlType: AadlType,
                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): DatatypeProviderPlugin = {
    return getDatatypeProviders(plugins).filter((p: DatatypeProviderPlugin) =>
      p.canHandleDatatypeProvider(aadlType, resolvedAnnexSubclauses))(0)
  }


  @memoize def getBridgeCodeProviders(plugins: MSZ[Plugin]): BridgeCodeProviderPlugin = {
    val ret = plugins.filter((p: Plugin) => p.isInstanceOf[BridgeCodeProviderPlugin]).map((p: Plugin) => p.asInstanceOf[BridgeCodeProviderPlugin])
    if (ret.size != 1) {
      halt("Only the default bridge code provider is currently allowed")
    }
    return ret(0)
  }


  @memoize def getEntryPointProviders(plugins: MSZ[Plugin]): MSZ[EntryPointProviderPlugin] = {
    return plugins.filter((p: Plugin) => p.isInstanceOf[EntryPointProviderPlugin]).map((p: Plugin) => p.asInstanceOf[EntryPointProviderPlugin])
  }

  def getEntryPointProvider(plugins: MSZ[Plugin],
                            component: AadlThreadOrDevice,
                            resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): EntryPointProviderPlugin = {
    val ret = getEntryPointProviders(plugins).filter((ep: EntryPointProviderPlugin) => ep.canHandleEntryPointProvider(component, resolvedAnnexSubclauses))
    if (ret.isEmpty) {
      halt("infeasible")
    }
    // first ep provider who can handle component wins
    return ret(0)
  }


  @memoize def getBehaviorProviders(plugins: MSZ[Plugin]): MSZ[BehaviorProviderPlugin] = {
    return plugins.filter((p: Plugin) => p.isInstanceOf[BehaviorProviderPlugin]).map((p: Plugin) => p.asInstanceOf[BehaviorProviderPlugin])
  }

  def canHandleBehaviorProviders(plugins: MSZ[Plugin],
                                 component: AadlThreadOrDevice,
                                 resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = {
    for (bp <- getBehaviorProviders(plugins) if bp.canHandleBehaviorProvider(component, resolvedAnnexSubclauses)) {
      return T
    }
    return F
  }
}

@msig trait ArsitPlugin extends Plugin

@msig trait BehaviorProviderPlugin extends ArsitPlugin {

  @pure def canHandleBehaviorProvider(component: AadlThreadOrDevice,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  // allows a plugin to provide its own behavior code implementation for component
  def handleBehaviorProvider(component: AadlThreadOrDevice,
                             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                             suggestedFilename: String,
                             componentDirectory: ISZ[String],

                             symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,

                             reporter: Reporter): ISZ[FileResource]
}

object BehaviorEntryPointProviderPlugin {

  @strictpure def emptyObjectContributions: ObjectContributions =
    ObjectContributions(ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())

  @strictpure def emptyPartialContributions: PartialMethodContributions =
    PartialMethodContributions(ISZ(), ISZ(), ISZ(), ISZ(), None(), None(), ISZ(), ISZ(), ISZ(), ISZ())

  @strictpure def emptyFullContributions: FullMethodContributions =
    FullMethodContributions(ISZ(), ISZ(), ISZ(), ISZ(), st"", ISZ(), ISZ(), ISZ(), ISZ())

  @sig trait BehaviorEntryPointContributions

  @sig trait BehaviorEntryPointObjectContributions extends BehaviorEntryPointContributions {
    def tags: ISZ[String]

    def imports: ISZ[String]

    def preObjectBlocks: ISZ[ST]

    def preMethodBlocks: ISZ[ST]

    def postMethodBlocks: ISZ[ST]

    def postObjectBlocks: ISZ[ST]

    def resources: ISZ[FileResource]
  }

  @datatype class ObjectContributions(val tags: ISZ[String],
                                      val imports: ISZ[String],
                                      val preObjectBlocks: ISZ[ST],
                                      val preMethodBlocks: ISZ[ST],
                                      val postMethodBlocks: ISZ[ST],
                                      val postObjectBlocks: ISZ[ST],
                                      val resources: ISZ[FileResource]) extends BehaviorEntryPointObjectContributions

  @sig trait BehaviorEntryPointMethodContributions extends BehaviorEntryPointObjectContributions {
    def markers: ISZ[Marker]
  }

  // allows plugin to fully provide the behavior code for a method.  An error will be thrown
  // if there are more than one of these plugins for a pipeline, or if BehaviorEntryPointPartialContributions
  // contributes a body or contract block
  @datatype class FullMethodContributions(val tags: ISZ[String],
                                          val imports: ISZ[String],
                                          val preObjectBlocks: ISZ[ST],

                                          val preMethodBlocks: ISZ[ST],
                                          val method: ST, // includes the method sig, optional contract, and body
                                          val postMethodBlocks: ISZ[ST],

                                          val postObjectBlocks: ISZ[ST],

                                          val markers: ISZ[Marker],
                                          val resources: ISZ[FileResource]) extends BehaviorEntryPointMethodContributions

  // allows plugin to provide parts of the behavior code for a method that will be
  // combined with those from other plugins in the same pipeline.
  // ContractBlock types must be the same for all plugins (all return cases or general)
  // an error will be thrown if more than one plugin provides an optBody
  @datatype class PartialMethodContributions(val tags: ISZ[String],
                                             val imports: ISZ[String],
                                             val preObjectBlocks: ISZ[ST],

                                             val preMethodBlocks: ISZ[ST],

                                             val contractBlock: Option[ContractBlock],

                                             val optBody: Option[ST],

                                             val postMethodBlocks: ISZ[ST],

                                             val postObjectBlocks: ISZ[ST],

                                             val markers: ISZ[Marker],
                                             val resources: ISZ[FileResource]) extends BehaviorEntryPointMethodContributions

  @sig trait ContractBlock

  @datatype class CaseContractBlock(val imports: ISZ[String],
                                    val cases: ISZ[ST]) extends ContractBlock

  @datatype class NonCaseContractBlock(val imports: ISZ[String],
                                       val contractReads: ISZ[ST],
                                       val contractRequires: ISZ[ST],
                                       val contractModifies: ISZ[ST],
                                       val contractEnsures: ISZ[ST],
                                       val contractFlows: ISZ[ST]) extends ContractBlock
}

@msig trait BehaviorEntryPointProviderPlugin extends ArsitPlugin {

  @pure def canBehaviorHandleEntryPointProvider(entryPoint: EntryPoints.Type, // the entry point
                                                optInEventPort: Option[AadlPort], // handler's in event port

                                                component: AadlThreadOrDevice,
                                                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                                                arsitOptions: ArsitOptions,
                                                symbolTable: SymbolTable,
                                                aadlTypes: AadlTypes): B

  // allows a plugin to provide contributions to the generated code for
  // an entrypoint (ie. where developers will add behavior code).
  // BehaviorEntryPointProviderPlugins will not be called
  // for a component if a BehaviorProviderPlugin has already handled the
  // component
  def handleBehaviorEntryPointProvider(entryPoint: EntryPoints.Type, // the entry point
                                       optInEventPort: Option[AadlPort], // handler's in event port
                                       component: AadlThreadOrDevice,
                                       componentNames: NameProvider,
                                       excludeComponentImplementation: B,

                                       methodSignature: String, // e.g. def handlePortName(value: PortType, api: ApiType): Unit
                                       defaultMethodBody: ST,

                                       resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                       basePackageName: String,
                                       symbolTable: SymbolTable,
                                       aadlTypes: AadlTypes,
                                       projectDirectories: ProjectDirectories,
                                       arsitOptions: ArsitOptions,
                                       reporter: Reporter): BehaviorEntryPointContributions

  // Called prior to codegen writing out the behavior code for the component.
  // This allows plugins the ability, for e.g., to write out blocks they've been
  // collecting to an external resource.
  def finaliseBehaviorEntryPointProvider(component: AadlThreadOrDevice,
                                         nameProvider: NameProvider,
                                         resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                         basePackageName: String,
                                         symbolTable: SymbolTable,
                                         aadlTypes: AadlTypes,
                                         projectDirectories: ProjectDirectories,
                                         arsitOptions: ArsitOptions,
                                         reporter: Reporter): Option[ObjectContributions] = {
    return None()
  }
}

@msig trait BridgeCodeProviderPlugin extends ArsitPlugin {
  def generate(nameProvider: NameProvider,
               component: AadlThreadOrDevice,
               ports: ISZ[Port],

               entryPointProvider: EntryPointProviderPlugin,

               symbolTable: SymbolTable,
               aadlTypes: AadlTypes,

               reporter: Reporter): BridgeCodeContributions
}

@msig trait EntryPointProviderPlugin extends ArsitPlugin {
  @pure def canHandleEntryPointProvider(component: AadlThreadOrDevice,
                                        resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  def handleEntryPointProvider(component: AadlThreadOrDevice,
                               nameProvider: NameProvider,
                               ports: ISZ[Port],

                               entryPointTemplate: EntryPointTemplate,

                               symbolTable: SymbolTable,
                               aadlTypes: AadlTypes,

                               reporter: Reporter): EntryPointContributions
}

@datatype class DatatypeContribution(val datatype: FileResource,
                                     val resources: ISZ[FileResource])

@msig trait DatatypeProviderPlugin extends ArsitPlugin {
  @pure def canHandleDatatypeProvider(aadlType: AadlType,
                                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B

  def handleDatatypeProvider(aadlType: AadlType,

                             datatypeTemplate: IDatatypeTemplate,

                             suggestFilename: String,
                             dataDirectory: String,

                             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                             symbolTable: SymbolTable,
                             aadlTypes: AadlTypes,

                             reporter: Reporter): DatatypeContribution
}


