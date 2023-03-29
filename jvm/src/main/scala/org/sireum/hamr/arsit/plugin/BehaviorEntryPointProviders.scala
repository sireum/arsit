// #Sireum

package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.arsit.templates.StringTemplate
import org.sireum.hamr.codegen.common.CommonUtil.toolName
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThreadOrDevice, AnnexClauseInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.Direction
import org.sireum.message.Reporter

object BehaviorEntryPointProviders {

  @strictpure def getPlugins(plugins: ISZ[Plugin]): ISZ[BehaviorEntryPointProviderPlugin] =
    plugins.filter(f => f.isInstanceOf[BehaviorEntryPointProviderPlugin]).map(m => m.asInstanceOf[BehaviorEntryPointProviderPlugin])

  def offer(entryPoint: EntryPoints.Type, optInEventPort: Option[AadlPort],
            m: AadlThreadOrDevice, excludeImpl: B, methodSig: String, defaultMethodBody: ST,
            annexClauseInfos: ISZ[AnnexClauseInfo],
            plugins: ISZ[BehaviorEntryPointProviderPlugin],

            basePackageName: String,
            symbolTable: SymbolTable,
            aadlTypes: AadlTypes,
            projectDirs: ProjectDirectories,
            reporter: Reporter): BehaviorEntryPointFullContributions = {

    var ret = BehaviorEntryPointFullContributions.empty
    var cases: ISZ[CaseContractBlock] = ISZ()
    var noncases: ISZ[NonCaseContractBlock] = ISZ()
    var optBody: Option[ST] = None()
    for(p <- plugins if p.canHandle(entryPoint, optInEventPort, m, annexClauseInfos) && !reporter.hasError) {
      p.handle(entryPoint, optInEventPort, m, excludeImpl, methodSig, defaultMethodBody, annexClauseInfos, basePackageName, symbolTable, aadlTypes, projectDirs, reporter) match {
        case b: BehaviorEntryPointFullContributions =>
          if (plugins.size > 1) {
            reporter.error(None(), toolName, "BehaviorEntryPointFullContributions cannot be combined with other behavior entry point provider plugins")
          }
          return b
        case b: BehaviorEntryPointPartialContributions =>
          if (b.optBody.nonEmpty) {
            if (optBody.nonEmpty) {
              reporter.error(None(), toolName, "A behavior entry point plugin has already contributed a method body")
            }
            optBody = b.optBody
          }
          ret = ret(
            tags = ret.tags ++ b.tags,
            imports = ret.imports ++ b.imports,
            preObjectBlocks = ret.preObjectBlocks ++ b.preObjectBlocks,
            preMethodBlocks = ret.preMethodBlocks ++ b.preMethodBlocks,
            postMethodBlocks = ret.postMethodBlocks ++ b.postMethodBlocks,
            postObjectBlocks = ret.postObjectBlocks ++ b.postObjectBlocks,
            markers = ret.markers ++ b.markers,
            resources = ret.resources ++ b.resources
          )
          b.contractBlock match {
            case Some(i: CaseContractBlock) => cases = cases :+ i
            case Some(i: NonCaseContractBlock) => noncases = noncases :+ i
            case _ =>
          }
          if(cases.nonEmpty && noncases.nonEmpty) {
            reporter.error(None(), toolName, "Logika doesn't support a mix of contract cases and general contracts")
          }
      }
    }

    @pure def processNonCases(entries: ISZ[NonCaseContractBlock]): ST = {
      @strictpure def wrap(prefix: String, es: ISZ[ST], addComma: B): Option[ST] =
        if(es.isEmpty) None()
        else Some(st"""$prefix(
                      |  ${(es, ",\n")}
                      |)${if(addComma)"," else ""}""")

      return (st"""Contract(
                  |  ${wrap("Reads", entries.flatMap(f => f.contractReads), T)}
                  |  ${wrap("Requires", entries.flatMap(f => f.contractRequires), T)}
                  |  ${wrap("Modifies", entries.flatMap(f => f.contractModifies), T)}
                  |  ${wrap("Ensures", entries.flatMap(f =>f.contractEnsures), T)}
                  |  ${wrap("InfoFlows", entries.flatMap(f => f.contractFlows), F)}
                  |)""")
    }


    val optContract: Option[ST] =
      if(cases.nonEmpty ) Some(
        st"""Contract(
            |  ${(cases, ",\n")}
            |)""")
      else if (noncases.nonEmpty) Some(processNonCases(noncases))
      else None()

    val body: ST = if(optBody.nonEmpty) optBody.get else defaultMethodBody
    val method =
      st"""$methodSig = {
          |  $optContract
          |  $body
          |}"""
    return ret(method = method)
  }


  @pure def portApiUsage(p: AadlPort): ST = {
    val portType: (AadlType, String) = p match {
      case aep: AadlEventPort => (TypeUtil.EmptyType, "event")
      case aedp: AadlEventDataPort => (aedp.aadlType, "event data")
      case adp: AadlDataPort => (adp.aadlType, "data")
      case _ => halt("Infeasible")
    }
    if (p.direction == Direction.In) {
      val typeName = portType._1.nameProvider.qualifiedReferencedTypeName
      return (
        st"""val apiUsage_${p.identifier}: Option[${typeName}] = api.get_${p.identifier}()
            |api.logInfo(s"Received on ${portType._2} port ${p.identifier}: $${apiUsage_${p.identifier}}")""")
    } else {
      val payload: String =
        if (portType._1.nameProvider.isEmptyType) ""
        else portType._1.nameProvider.example()

      return st"api.put_${p.identifier}($payload)"
    }
  }

  def genComputeMethodBody(optInEventPort: Option[AadlPort],
                           context: AadlThreadOrDevice,
                           includeApiUsage: B): ST = {

    val inPorts: ISZ[AadlPort] = context.features.filter(f => f.isInstanceOf[AadlPort] && f.asInstanceOf[AadlPort].direction == Direction.In).map(m => m.asInstanceOf[AadlPort])

    val exampleApiGetterUsage: ST =
        st"""// example api usage
            |
            |${(inPorts.map((p: AadlPort) => portApiUsage(p)), "\n")}"""

    val ret: ST = optInEventPort match {
      case Some(p) =>
        val feedback: ST = p match {
          case a:AadlEventDataPort => st"""api.logInfo(s"  received $$value")"""
          case a:AadlEventPort => st"""api.logInfo("  received event")"""
          case _ => halt("Infeasible as we're generating a handler method for an event port")
        }
        val methodName = genMethodName(EntryPoints.compute, optInEventPort)
        st"""api.logInfo("example $methodName implementation")
            |$feedback
            |${if (includeApiUsage) Some(exampleApiGetterUsage) else None[ST]()}"""
      case _ => // must be time triggered method
        if(includeApiUsage) exampleApiGetterUsage else st""
    }

    return ret
  }

  def genMethodBody(entryPoint: EntryPoints.Type,
                    context: AadlThreadOrDevice): ST = {

      val outPorts: ISZ[AadlPort] = context.features.filter(f => f.isInstanceOf[AadlPort] && f.asInstanceOf[AadlPort].direction == Direction.Out).map(m => m.asInstanceOf[AadlPort])

    val ret: ST = entryPoint match {
        case EntryPoints.compute => halt("Infeasible - call genComputeMethodBody")
        case EntryPoints.initialise =>
          val o: Option[ST] =
            if (outPorts.nonEmpty) Some(
              st"""
                  |${(outPorts.map((p: AadlPort) => portApiUsage(p)), "\n")}""")
            else None()
          st"""// example api usage
              |
              |api.logInfo("Example info logging")
              |api.logDebug("Example debug logging")
              |api.logError("Example error logging")
              |${o}"""
        case _ =>
          st""
      }
      return ret
  }

  def genMethodName(entryPoint: EntryPoints.Type, optInEventPort: Option[AadlPort]): String = {
    entryPoint match {
      case EntryPoints.compute =>
        optInEventPort match {
          case Some(aep: AadlEventPort) => return s"handle_${aep.identifier}"
          case Some(aedp: AadlEventDataPort) => return s"handle_${optInEventPort.get.identifier}"
          case _ => return s"timeTriggered"
        }
      case _ => return entryPoint.name
    }
  }

  def genMethodSignature(entryPoint: EntryPoints.Type, names: NameProvider, optInEventPort: Option[AadlPort]): String = {
    val methodName = genMethodName(entryPoint, optInEventPort)
    val args: ISZ[String] = entryPoint match {
      case EntryPoints.compute =>
        optInEventPort match {
          case Some(aep: AadlEventPort) => ISZ(s"api: ${names.apiOperational}")
          case Some(aedp: AadlEventDataPort) =>
            ISZ(s"api: ${names.apiOperational}", s"value: ${aedp.aadlType.nameProvider.qualifiedReferencedTypeName}")
          case _ => ISZ(s"api: ${names.apiOperational}")
        }
      case EntryPoints.initialise => ISZ(s"api: ${names.apiInitialization}")
      case _ => ISZ(s"api: ${names.apiOperational}")
    }
    return st"def $methodName(${(args, ", ")}): Unit".render
  }

  def genComponentImpl(names: NameProvider, entries: ISZ[BehaviorEntryPointFullContributions]): ST = {
    @strictpure def wrapString(v: ISZ[String], sep: String): Option[ST] = if(v.isEmpty) None() else Some(st"${(v, sep)}")
    @strictpure def wrapST(v: ISZ[ST], sep: String): Option[ST] = if(v.isEmpty) None() else Some(st"${(v, sep)}")

    val body = wrapST(entries.flatMap((f: BehaviorEntryPointFullContributions) => f.preMethodBlocks) ++
      entries.map((f: BehaviorEntryPointFullContributions) => f.method) ++
      entries.flatMap((f: BehaviorEntryPointFullContributions) => f.postMethodBlocks), "\n\n")

    // remove duplicate tags
    val tags: Set[String] = (Set.empty + "#Sireum") ++ entries.flatMap((f: BehaviorEntryPointFullContributions) => f.tags)

    return (
    st"""// ${(tags.elements, " ")}
        |
        |package ${names.packageName}
        |
        |import org.sireum._
        |import ${names.basePackage}._
        |${wrapString(entries.flatMap((f: BehaviorEntryPointFullContributions) => f.imports), "\n")}
        |
        |${wrapST(entries.flatMap((f: BehaviorEntryPointFullContributions) => f.preObjectBlocks), "\n\n")}
        |${StringTemplate.safeToEditComment()}
        |object ${names.componentSingletonType} {
        |
        |  $body
        |}
        |${wrapST(entries.flatMap((f: BehaviorEntryPointFullContributions) => f.postObjectBlocks), "\n\n")}
        |""")
  }
}
