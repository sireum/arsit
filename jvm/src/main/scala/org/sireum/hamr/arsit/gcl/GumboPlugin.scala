// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.{BehaviorEntryPointContributions, BehaviorEntryPointPartialContributions, BehaviorEntryPointProviderPlugin, NonCaseContractBlock}
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.message.Reporter

@record class GumboPlugin extends BehaviorEntryPointProviderPlugin {

  val name: String = "Gumbo Plugin"

  var handledStateVars: B = F
  var handledMethods: B = F

  def canHandle(entryPoint: EntryPoints.Type,
                optInEventPort: Option[AadlPort],
                component: AadlThreadOrDevice,
                resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, _)) =>
        return (annex.state.nonEmpty && !handledStateVars) ||
          (annex.methods.nonEmpty && !handledMethods) ||
          (entryPoint == EntryPoints.initialise && annex.initializes.nonEmpty) ||
          (entryPoint == EntryPoints.compute && annex.compute.nonEmpty)
      case _ => return F
    }
  }

  def handle(entryPoint: EntryPoints.Type,
             optInEventPort: Option[AadlPort], // will be populated if processing the event handler for a sporadic component
             component: AadlThreadOrDevice,
             excludeComponentImplementation: B,
             methodSignature: String,
             defaultMethodBody: ST,
             resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

             basePackageName: String,
             symbolTable: SymbolTable,
             aadlTypes: AadlTypes,
             projectDirectories: ProjectDirectories,
             reporter: Reporter): BehaviorEntryPointContributions = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>
        var stateVars: ISZ[ST] = ISZ()
        var markers: ISZ[Marker] = ISZ()
        var reads: ISZ[ST] = ISZ()
        var requires: ISZ[ST] = ISZ()
        var modifies: ISZ[ST] = ISZ()
        var ensures: ISZ[ST] = ISZ()
        var flows: ISZ[ST] = ISZ()

        if (annex.state.nonEmpty) { //} && !handledStateVars) {
          val p = GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processStateVars(annex.state)
          stateVars = stateVars :+ p._1
          markers = markers :+ p._2
          handledStateVars = T
        }
        if (annex.methods.nonEmpty) { //} && !handledMethods) {
          println("gcl methods")
          handledMethods = T
        }
        if (annex.initializes.nonEmpty) {
          val r = GumboGen.processInitializes(component, symbolTable, aadlTypes, basePackageName).get
          requires = requires ++ r.requires
          modifies = modifies ++ r.modifies
          ensures = ensures ++ r.ensures
          flows = flows ++ r.flows
        }
        if (annex.compute.nonEmpty) {
          println("compute")
          GumboGen(gclSymbolTable, symbolTable, aadlTypes, basePackageName).processCompute2(annex.compute.get, optInEventPort, component) match {
            case (n: NonCaseContractBlock, mmarkers) =>
              markers = markers ++ mmarkers
              reads = reads ++ n.contractReads
              requires = requires ++ n.contractRequires
              modifies = modifies ++ n.contractModifies
              ensures = ensures ++ n.contractEnsures
              flows = flows ++ n.contractFlows
            case _ => halt("Infeasible for now")
          }
        }

        val contractBlock = NonCaseContractBlock(reads, requires, modifies, ensures, flows)
        val ret = BehaviorEntryPointPartialContributions.empty
        return ret(preMethodBlocks = stateVars, markers = markers, contractBlock = Some(contractBlock))
      case _ => halt("Infeasible")
    }
  }
}
