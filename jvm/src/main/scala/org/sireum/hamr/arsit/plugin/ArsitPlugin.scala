// #Sireum
package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.arsit.bts.BlessBehaviorProviderPlugin
import org.sireum.hamr.arsit.templates.EntryPointTemplate
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.message.Reporter

object ArsitPlugin {

  val defaultPlugins: ISZ[Plugin] = ISZ(BlessBehaviorProviderPlugin(), SingletonBridgeCodeProviderPlugin(), SingletonEntryPointProviderPlugin())

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

  def handle(component: AadlThreadOrDevice,
             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
             suggestedFilename: String,
             componentDirectory: ISZ[String],

             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,

             reporter: Reporter): ISZ[Resource]
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

@sig trait DatatypeProviderPlugin extends ArsitPlugin {
  @pure def canHandle(): B

  def handle(): ISZ[Resource]
}

