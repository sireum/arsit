// #Sireum

package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.templates.{DatatypeTemplate, EnumTemplate, IDatatypeTemplate}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.IResource
import org.sireum.hamr.codegen.common.symbols.{AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types._
import org.sireum.message.Reporter

object DefaultDatatypeProvider {
  @pure def genDefaultTemplate(aadlType: AadlType): IDatatypeTemplate = {
    if (aadlType.isInstanceOf[BaseType]) {
      halt(s"Support for customizing base type ${aadlType.name} hasn't been implemented yet")
    }

    aadlType match {
      case e: EnumType => return EnumTemplate(e, e.values, T)
      case e: RecordType => return DatatypeTemplate(e, T)
      case e: ArrayType => return DatatypeTemplate(e, T)
      case e: TODOType => return DatatypeTemplate(e, F)
    }
  }
}

@record class DefaultDatatypeProvider extends DatatypeProviderPlugin {
  @strictpure def name: String = "Default Datatype Provider"

  @strictpure def canHandle(aadlType: AadlType, resolvedAnnexSubclauses: ISZ[AnnexClauseInfo]): B = T

  @pure def handle(aadlType: AadlType,
                   datatypeTemplate: IDatatypeTemplate,
                   suggestFilename: String,
                   dataDirectory: String,
                   resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                   symbolTable: SymbolTable,
                   aadlTypes: AadlTypes,
                   reporter: Reporter): DatatypeContribution = {

    aadlType match {
      case at: ArrayType if at.dimensions.size > 1 =>
        reporter.error(None(), CommonUtil.toolName, s"Codegen currently only supports single dimension arrays, but ${aadlType.name} specifies a ${at.dimensions.size} dimensional array")
      case _ =>
    }
    return DatatypeContribution(
      datatype = IResource(
        dstPath = s"$dataDirectory/$suggestFilename", // TODO: need fileSep
        content = datatypeTemplate.generateDefault(),
        markers = ISZ(),
        overwrite = datatypeTemplate.willBeOverwritten,
        makeExecutable = F,
        makeCRLF = F,
        isDatatype = T),
      resources = ISZ()
    )
  }
}
