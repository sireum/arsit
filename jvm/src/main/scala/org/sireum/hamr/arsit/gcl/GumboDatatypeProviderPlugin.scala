// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.{DatatypeContribution, DatatypeProviderPlugin}
import org.sireum.hamr.arsit.templates.{DatatypeTemplate, EnumTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.common.containers.IResource
import org.sireum.hamr.codegen.common.symbols.{AnnexClauseInfo, GclAnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.message.Reporter

@record class GumboDatatypeProviderPlugin extends DatatypeProviderPlugin {
  @strictpure def name: String = "GUMBO Datatype Provider Plugin"

  @strictpure def canHandleDatatypeProvider(aadlType: AadlType, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B =
    resolvedAnnexSubclauses.filter((f: AnnexClauseInfo) => f.isInstanceOf[GclAnnexClauseInfo]).nonEmpty

  @pure def handleDatatypeProvider(aadlType: AadlType,
                                   datatypeTemplate: IDatatypeTemplate,

                                   suggestFilename: String,
                                   dataDirectory: String,

                                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                                   symbolTable: SymbolTable,
                                   aadlTypes: AadlTypes,

                                   reporter: Reporter): DatatypeContribution = {

    val subclauses = resolvedAnnexSubclauses.filter((f: AnnexClauseInfo) => f.isInstanceOf[GclAnnexClauseInfo])
    if (subclauses.size != 1) {
      // should be infeasible as sym resolution should have rejected this already
      halt(s"A data component can have at most one GUMBO subclause but ${aadlType.name} has ${subclauses.size}")
    }

    datatypeTemplate match {
      case dt: DatatypeTemplate =>
        val invariant = GumboGen.processInvariants(aadlType, symbolTable, aadlTypes, aadlType.nameProvider.basePackageName)

        val imports = GumboGen.imports // TODO: should be returned

        val datatypeBlocks = dt.defaultDatatypeBlocks ++ invariant

        val useDefault: ISZ[String] = ISZ()
        val content = dt.generateCustom(
          custSlangSwitches = useDefault,
          custImports = (Set.empty[String] ++ imports).elements,
          custDatatypeCompanionBlocks = useDefault,
          custParams = useDefault,
          custDatatypeBlocks = datatypeBlocks.map((m: ST) => m.render),
          custPayloadSingletonBlocks = useDefault,
          custPreBlocks = useDefault,
          custPostBlocks = useDefault
        )

        return DatatypeContribution(
          datatype = IResource(
            dstPath = s"${dataDirectory}/${suggestFilename}",
            content = content,
            markers = ISZ(),
            overwrite = T,
            makeExecutable = F,
            makeCRLF = F,
            isDatatype = T),
          resources = ISZ())

      case en: EnumTemplate =>
        halt(s"Not expecting GUMBO contracts on enum types: ${aadlType.name}")
    }
  }
}
