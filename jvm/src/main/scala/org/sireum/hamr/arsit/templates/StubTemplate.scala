// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.gcl.GumboGen.{GclEntryPointInitialize, GclEntryPointPeriodicCompute, GclEntryPointSporadicCompute}
import org.sireum.hamr.arsit.{EntryPoints, Port}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.symbols.{AadlPort, Dispatch_Protocol, SymbolTable}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.FeatureCategory

object StubTemplate {

  def addImports(imports: ISZ[String]): Option[ST] = {
    val s: Set[String] = Set.empty[String] ++ (for(i <- imports) yield s"import $i")
    return if (s.nonEmpty) Some(st"${(s.elements, "\n")}") else None()
  }

  @pure def addId(s: String): String = {
    return s"${s}_Id"
  }

  @pure def putValue(p: Port): ST = {
    return st"""Art.putValue(${addId(p.name)}, ${p.getPortTypeNames.qualifiedPayloadName}${if (p.getPortTypeNames.isEmptyType) "()" else "(value)"})"""
  }

  @pure def getterApi(p: Port): ST = {
    val isEvent = CommonUtil.isAadlEventPort(p.feature)
    val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
    val payloadType: String = if (isEvent) "Empty" else p.getPortTypeNames.qualifiedPayloadName
    val _match: String = if (isEvent) "Empty()" else s"${payloadType}(v)"
    val value: String = if (isEvent) "Empty()" else "v"

    val ret: ST =
      st"""def get_${p.name}() : Option[${typeName}] = {
          |  val value : Option[${typeName}] = Art.getValue(${addId(p.name)}) match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) =>
          |      Art.logError(id, s"Unexpected payload on port ${p.name}.  Expecting '${payloadType}' but received $${v}")
          |      None[${typeName}]()
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}"""
    return ret
  }

  @pure def portApiUsage(p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      return (
        st"""val apiUsage_${p.name}: Option[${typeName}] = api.get_${p.name}()
            |api.logInfo(s"Received on ${p.name}: $${apiUsage_${p.name}}")""")
    } else {
      val payload: String =
        if (p.getPortTypeNames.isEmptyType) ""
        else p.getPortTypeNames.example()

      return st"api.put_${p.name}($payload)"
    }
  }

  @pure def portCase(cname: String, v: Port, operationalApiType: String, first: B): ST = {
    val apiId = ApiTemplate.apiOperationalId
    val methodName = s"handle_${v.name}"
    val cMethodName = s"${cname}.${methodName}"

    v.feature.category match {
      case FeatureCategory.EventDataPort =>

        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}){
              |  val Some(${v.getPortTypeNames.qualifiedPayloadName}(value)) = Art.getValue(${addId(v.name)})
              |
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}, value: ${v.getPortTypeNames.qualifiedReferencedTypeName}): Unit = {}
              |  ${cMethodName}(${apiId}, value)
              |}"""
        return ret
      case FeatureCategory.EventPort =>
        val ret: ST =
          st"""${if (!first) "else " else ""}if(portId == ${addId(v.name)}) {
              |  // implement the following in 'component':  def ${methodName}(api: ${operationalApiType}): Unit = {}
              |  ${cMethodName}(${apiId})
              |}"""
        return ret
      case _ => halt(s"Unexpected ${v.feature.category}")
    }
  }

  @pure def componentImplBlock(componentType: String,
                               names: NameProvider,
                               dispatchProtocol: Dispatch_Protocol.Type,
                               ports: ISZ[Port],
                               excludeComponentImpl: B,
                               preBlocks: ISZ[ST],
                               entryPointContracts: Map[EntryPoints.Type, GumboGen.GclEntryPointContainer],
                               symbolTable: SymbolTable): ST = {

    val inPorts = ports.filter(p => CommonUtil.isInPort(p.feature))
    val outPorts = ports.filter(p => CommonUtil.isOutPort(p.feature))

    def genMethod(signature: String, body: Option[ST]): ST = {
      val ret: ST = {
        if (body.nonEmpty) {
          st"""def ${signature}: Unit = {
              |  ${body}
              |}"""
        } else {
          st"def ${signature}: Unit = { }"
        }
      }
      return ret
    }

    val initSig: String = s"${EntryPoints.initialise.string}(api: ${names.apiInitialization})"
    val contract: Option[ST] = entryPointContracts.get(EntryPoints.initialise) match {
      case Some(x: GclEntryPointInitialize) => Some(x.contract)
      case Some(x) => halt(s"Infeasible ${x}")
      case _ => None()
    }

    val init: ST =
      if (excludeComponentImpl) {
        genMethod(initSig, contract)
      }
      else {
        val o: Option[ST] =
          if (outPorts.nonEmpty) Some(
            st"""
                                         |${(outPorts.map((p: Port) => portApiUsage(p)), "\n")}""")
          else None()
        genMethod(initSig, Some(
          st"""${contract}
              |// example api usage
              |
              |api.logInfo("Example info logging")
              |api.logDebug("Example debug logging")
              |api.logError("Example error logging")
              |${o}"""))
      }

    val exampleApiGetterUsage: Option[ST] =
      if (excludeComponentImpl) {
        None()
      }
      else {
        Some(
          st"""// example api usage
              |
              |${(inPorts.map((p: Port) => portApiUsage(p)), "\n")}""")
      }

    def comma(oo: Option[ST]): Option[ST] = {
      return if (oo.isEmpty) None() else Some(st"${oo.get},")
    }

    val eventHandlers: ISZ[ST] =
      if (dispatchProtocol == Dispatch_Protocol.Periodic) {
        val optContract: Option[ST] = entryPointContracts.get(EntryPoints.compute) match {
          case Some(g: GclEntryPointPeriodicCompute) =>
            Some(
              st"""Contract(
                  |  ${comma(g.requires)}
                  |  ${comma(g.modifies)}
                  |  ${comma(g.ensures)}
                  |  ${g.flows}
                  |)""")
          case _ => None()
        }

        val ttsig: String = s"timeTriggered(api: ${names.apiOperational})"
        ISZ(if (excludeComponentImpl) genMethod(ttsig, optContract)
        else genMethod(ttsig, Some(
          st"""$optContract
              |$exampleApiGetterUsage""")))
      } else {
        val inEventPorts = ports.filter(p => CommonUtil.isInFeature(p.feature) && CommonUtil.isEventPort(p.feature))
        var first = T
        inEventPorts.map(p => {
          val aadlPort = symbolTable.featureMap.get(p.feature.identifier.name).get.asInstanceOf[AadlPort]

          val handlerName: String = s"handle_${p.name}"

          val (eventSig, receivedFeedback): (String, String) = p.feature.category match {
            case FeatureCategory.EventDataPort =>
              val edpSig = s"$handlerName(api: ${names.apiOperational}, value : ${p.getPortTypeNames.qualifiedReferencedTypeName})"
              (edpSig, s"${"${value}"}")
            case FeatureCategory.EventPort =>
              val epSig = s"$handlerName(api: ${names.apiOperational})"
              (epSig, s"${p.name} event")
            case _ => halt(s"Unexpected ${p.feature.category}")
          }

          val optContract: Option[ST] = entryPointContracts.get(EntryPoints.compute) match {
            case Some(g: GclEntryPointSporadicCompute) =>
              val handler = g.handlers.get(aadlPort).get

              Some(
                st"""Contract(
                    |  ${comma(handler.requires)}
                    |  ${comma(handler.modifies)}
                    |  ${comma(handler.ensures)}
                    |  ${handler.flows}
                    |)""")
            case _ => None()
          }

          if (excludeComponentImpl) {
            genMethod(eventSig, optContract)
          } else {
            val stringInterp: String = if (CommonUtil.isAadlEventPort(p.feature)) "" else "s"
            var body =
              st"""${optContract}
                  |api.logInfo("example $handlerName implementation")
                  |api.logInfo(${stringInterp}"received ${receivedFeedback}")"""
            if (first) { // only attach example api usage to first event handler
              first = F
              body =
                st"""$body
                    |${exampleApiGetterUsage}"""
            }
            genMethod(eventSig, Some(body))
          }
        })
      }

    val remainingEntryPoints =
      ISZ(EntryPoints.activate, EntryPoints.deactivate, EntryPoints.finalise, EntryPoints.recover).map((m: EntryPoints.Type) =>
        st"def ${m.string}(api: ${names.apiOperational}): Unit = { }")

    val preBlocksOpt: Option[ST] = if (preBlocks.isEmpty) None()
    else Some(
      st"""${(preBlocks, "\n")}
          |""")

    val ret: ST =
      st"""${StringTemplate.safeToEditComment()}
          |object $componentType {
          |
          |  ${preBlocksOpt}
          |  ${init}
          |
          |  ${(eventHandlers, "\n\n")}
          |
          |  ${(remainingEntryPoints, "\n\n")}
          |}"""
    return ret
  }

  @pure def subprogram(methodName: String,
                       params: ISZ[String],
                       returnType: Option[String],
                       exampleValue: Option[ST]): (ST, ST) = {
    val _returnType: String = if (returnType.nonEmpty) returnType.get else "Unit"
    return (
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = ${"$"}""",
      st"""def ${methodName}(${(params, ",\n")}): ${_returnType} = {
          |  ${if (exampleValue.nonEmpty) st"return ${exampleValue.get}" else ""}
          |}""")
  }

  @pure def slangPreamble(inSlang: B,
                          packageName: String,
                          topLevelPackageName: String,
                          imports: ISZ[String],
                          blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""${if (inSlang) "// #Sireum\n\n" else ""}package $packageName
          |
          |import org.sireum._
          |import ${topLevelPackageName}._
          |${addImports(imports)}
          |
          |${(blocks, "\n\n")}
          |"""
    return ret
  }

  @pure def slangBody(slangAnnotation: String,
                      objectName: String,
                      body: ISZ[ST]): ST = {
    val ret: ST =
      st"""${slangAnnotation}object ${objectName} {
          |
          |  ${(body, "\n\n")}
          |}"""
    return ret
  }
}

