// #Sireum

package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.ObjectContributions
import org.sireum.hamr.arsit.templates.{StringTemplate, StubTemplate}
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.IdPath
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.{GclCompute, GclInitialize}
import org.sireum.message.Reporter

@record class GumboXPlugin extends BehaviorEntryPointProviderPlugin {
  val name: String = "GumboX Plugin"

  var initComponentMap: Map[IdPath, ISZ[BehaviorEntryPointProviderPlugin.PartialMethodContributions]] = Map.empty
  var computeComponentMap: Map[IdPath, BehaviorEntryPointProviderPlugin.PartialMethodContributions] = Map.empty

  override def canHandle(entryPoint: EntryPoints.Type,
                         optInEventPort: Option[AadlPort],
                         component: AadlThreadOrDevice,
                         resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],
                         symbolTable: SymbolTable): B = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      // GCL's symbol resolver ensures there's at most one GCL clause per component
      case ISZ(GclAnnexClauseInfo(annex, _)) =>
        entryPoint match {
          case EntryPoints.initialise =>
            annex.initializes match {
              case Some(GclInitialize(_, guarantees, _)) => return guarantees.nonEmpty
              case _ => return F
            }
          case EntryPoints.compute =>
            annex.compute match {
              case Some(GclCompute(_, specs, cases, handlers, _)) =>
                return (specs.nonEmpty || cases.nonEmpty || handlers.nonEmpty) && !computeComponentMap.contains(component.path)
              case _ => return F
            }
          case _ => // the other entry points cannot have Gcl contracts
            return F
        }
      case _ => return F
    }
  }

  override def handle(entryPoint: EntryPoints.Type,
                      optInEventPort: Option[AadlPort],
                      component: AadlThreadOrDevice,
                      excludeComponentImplementation: B,
                      methodSignature: String,
                      defaultMethodBody: ST,
                      resolvedAnnexSubclauses: ISZ[AnnexClauseInfo],

                      basePackageName: String,
                      symbolTable: SymbolTable,
                      aadlTypes: AadlTypes,
                      projectDirectories: ProjectDirectories,
                      reporter: Reporter): BehaviorEntryPointProviderPlugin.BehaviorEntryPointContributions = {
    resolvedAnnexSubclauses.filter(p => p.isInstanceOf[GclAnnexClauseInfo]) match {
      case ISZ(GclAnnexClauseInfo(annex, gclSymbolTable)) =>
        entryPoint match {
          case EntryPoints.initialise if annex.initializes.nonEmpty =>

          case EntryPoints.compute if annex.compute.nonEmpty =>
            computeComponentMap = computeComponentMap + component.path ~>
              GumboXGen.processCompute2(component, annex, gclSymbolTable, symbolTable, aadlTypes, basePackageName)
          case _ => // gumbo contracts cannot be places on the other entrypoints
        }
        return BehaviorEntryPointProviderPlugin.emptyPartialContributions
      case _ => halt("Infeasible")
    }
  }

  override def finalise(component: AadlThreadOrDevice,
                        names: NameProvider,
                        basePackageName: String,
                        symbolTable: SymbolTable,
                        aadlTypes: AadlTypes,
                        projectDirectories: ProjectDirectories,
                        reporter: Reporter): Option[ObjectContributions] = {
    computeComponentMap.get(component.path) match {
      case Some(e) =>
        val content =
          st"""// #Sireum
              |
              |package ${names.packageName}
              |
              |import org.sireum._
              |import ${basePackageName}._
              |${StubTemplate.addImports(e.imports)}
              |
              |${StringTemplate.doNotEditComment(None())}
              |object ${names.bridge}_GumboX {
              |  ${(e.preMethodBlocks, "\n\n")}
              |}
              |"""

        val path = s"${projectDirectories.bridgeDir}/${names.packagePath}/${names.bridge}_GumboX.scala"
        val resource = ResourceUtil.createResource(path, content, T)

        return Some(BehaviorEntryPointProviderPlugin.emptyObjectContributions(resources = ISZ(resource)))
      case _ =>
        return None()
    }
  }
}
