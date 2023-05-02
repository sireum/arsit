// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.{BehaviorEntryPointContributions, NonCaseContractBlock}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories, Util}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.message.Reporter

@record class GumboPlugin extends BehaviorEntryPointProviderPlugin {

  val name: String = "Gumbo Plugin"

  var handledAnnexLibraries: B = F
  var handledStateVars: Set[AadlComponent] = Set.empty
  var handledSubClauseFunctions: Set[AadlComponent] = Set.empty

  @strictpure def getAnnexLibraries(symbolTable: SymbolTable): ISZ[GclAnnexLibInfo] =
    symbolTable.annexLibInfos.filter(f => f.isInstanceOf[GclAnnexLibInfo]).map(m => m.asInstanceOf[GclAnnexLibInfo])

  def canHandle(entryPoint: EntryPoints.Type,
                optInEventPort: Option[AadlPort],
                component: AadlThreadOrDevice,
                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                arsitOptions: ArsitOptions,
                symbolTable: SymbolTable,
                aadlTypes: AadlTypes): B = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      // GCL's symbol resolver ensures there's at most one GCL clause per component
      case ISZ(GclAnnexClauseInfo(annex, _)) =>
        return (!handledAnnexLibraries && getAnnexLibraries(symbolTable).nonEmpty) ||
          (annex.state.nonEmpty && !handledStateVars.contains(component)) ||
          (annex.methods.nonEmpty && !handledSubClauseFunctions.contains(component)) ||
          (entryPoint == EntryPoints.initialise && annex.initializes.nonEmpty) ||
          (entryPoint == EntryPoints.compute && annex.compute.nonEmpty)
      case _ => return F
    }
  }

  def handle(entryPoint: EntryPoints.Type,
             optInEventPort: Option[AadlPort], // will be populated if processing the event handler for a sporadic component
             component: AadlThreadOrDevice,
             componentNames: NameProvider,
             excludeComponentImplementation: B,
             methodSignature: String,
             defaultMethodBody: ST,
             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             basePackageName: String,
             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,
             projectDirectories: ProjectDirectories,
             arsitOptions: ArsitOptions,
             reporter: Reporter): BehaviorEntryPointContributions = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>
        var imports: ISZ[String] = ISZ()
        var preMethodBlocks: ISZ[ST] = ISZ()
        var markers: ISZ[Marker] = ISZ()
        var reads: ISZ[ST] = ISZ()
        var requires: ISZ[ST] = ISZ()
        var modifies: ISZ[ST] = ISZ()
        var ensures: ISZ[ST] = ISZ()
        var flows: ISZ[ST] = ISZ()
        var resources: ISZ[Resource] = ISZ()

        if (!handledAnnexLibraries) {
          for (gclLib <- getAnnexLibraries(symbolTable)) {
            val (content, filename) = GumboGen.processGclLibrary(gclLib, symbolTable, aadlTypes, basePackageName)
            resources = resources :+ ResourceUtil.createResource(
              Util.pathAppend(projectDirectories.componentDir, filename), content, T)
          }
          handledAnnexLibraries = T
        }

        if (annex.state.nonEmpty && !handledStateVars.contains(component)) {
          val p = GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processStateVars(annex.state)
          preMethodBlocks = preMethodBlocks :+ p._1
          markers = markers :+ p._2
          handledStateVars = handledStateVars + component
        }

        if (annex.methods.nonEmpty && !handledSubClauseFunctions.contains(component)) {
          val (content, marker) = GumboGen.processSubclauseFunctionsH(annex.methods, gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          preMethodBlocks = preMethodBlocks :+ content
          markers = markers :+ marker
          handledSubClauseFunctions = handledSubClauseFunctions + component
        }

        entryPoint match {
          case EntryPoints.initialise if annex.initializes.nonEmpty =>
            val r = GumboGen.processInitializes(component, symbolTable, aadlTypes, basePackageName).get
            requires = requires ++ r.requires
            modifies = modifies ++ r.modifies
            ensures = ensures ++ r.ensures
            flows = flows ++ r.flows
            markers = markers ++ r.markers
            imports = imports ++ r.imports

          case EntryPoints.compute if annex.compute.nonEmpty =>
            GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processCompute2(annex.compute.get, optInEventPort, component) match {
              case (n: NonCaseContractBlock, mmarkers) =>
                markers = markers ++ mmarkers
                reads = reads ++ n.contractReads
                requires = requires ++ n.contractRequires
                modifies = modifies ++ n.contractModifies
                ensures = ensures ++ n.contractEnsures
                flows = flows ++ n.contractFlows
                imports = imports ++ n.imports

              case _ => halt("Not handling Contract cases yet")
            }
          case _ => // gumbo contracts cannot currently be placed on the other entrypoints
        }

        val contractBlock = NonCaseContractBlock(imports, reads, requires, modifies, ensures, flows)
        val ret = BehaviorEntryPointProviderPlugin.emptyPartialContributions
        return ret(imports = imports, preMethodBlocks = preMethodBlocks, markers = markers, contractBlock = Some(contractBlock))
      case _ => halt("Infeasible")
    }
  }
}
