// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{SeL4NixTemplate, StringTemplate}
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir

object NixGen{
  val IPC_C: String = "ipc.c"
  val EXT_H: String = "ext.h"
  val EXT_C: String = "ext.c"

  val KNOWN_HAMR_PROVIDED_FILES: ISZ[String] = ISZ(IPC_C, EXT_H, EXT_C)
}

@msig trait NixGen {
  def dirs: ProjectDirectories

  def root: AadlSystem

  def arsitOptions: ArsitOptions

  def symbolTable: SymbolTable

  def types: AadlTypes

  def previousPhase: Result

  def generate(): ArsitResult

  def genExtensionEntries(c: ir.Component, names: Names, ports: ISZ[Port]): (ISZ[ST], ISZ[ST]) = {
    var extHEntries: ISZ[ST] = ISZ()
    var extCEntries: ISZ[ST] = ISZ()

    if(types.rawConnections) {
      // add numBit and numBytes global vars for each type passing between components

      val maxBitSize: Z = TypeUtil.getMaxBitsSize(symbolTable) match {
        case Some(z) => z
        case _ =>
          // model must only contain event ports (i.e not data ports)
          1
      }

      var seenTypes: Set[AadlType] = Set.empty
      for(p <- ports.filter(p => CommonUtil.isDataPort(p.feature))) {
        val originatingType: AadlType = p._portType match {
          case BitType(_, _, _, Some(o)) => o
          case _ => halt(s"Unexpected: Could not find originating type for ${p._portType} used by ${p.parentName}.${p.path}")
        }
        if(!seenTypes.contains(originatingType)) {

          val bitSize: Z = originatingType.bitSize match {
            case Some(z) => z
            case _ =>
              val msg = s"${originatingType.name} does not specify a bit size, assuming max bit size or ${maxBitSize}. Used by port ${p.parentName}.${p.name}"
              reporter.warn(None(), Util.toolName, msg)

              maxBitSize
          }

          seenTypes = seenTypes + originatingType

          val originatingTypeNames: DataTypeNames = Util.getDataTypeNames(originatingType, names.basePackage)

          val numBitsName = BitCodecNameUtil.numBitsConstName(originatingTypeNames.qualifiedCTypeName)
          val numBytesName = BitCodecNameUtil.numBytesConstName(originatingTypeNames.qualifiedCTypeName)

          extHEntries = extHEntries :+
            st"""// bit-codec size for ${originatingTypeNames.qualifiedCTypeName}
                |#define ${numBitsName} ${bitSize}
                |#define ${numBytesName} ((${numBitsName} - 1) / 8 + 1)"""
        }
      }

      extHEntries = extHEntries :+ SeL4NixTemplate.bitCodecExtHEnties()
      extCEntries = extCEntries :+ SeL4NixTemplate.bitCodecExtCEnties()
    }
    return (extHEntries, extCEntries)
  }

  def genExtensionFiles(c: ir.Component, names: Names, ports: ISZ[Port]): (ISZ[Os.Path], ISZ[Resource]) = {

    val rootExtDir = Os.path(dirs.cExt_c_Dir)

    var extensionFiles: ISZ[Os.Path] = ISZ()
    var resources: ISZ[Resource] = ISZ()

    if (arsitOptions.excludeImpl) {

      val componentName = names.cComponentType
      val extRoot = rootExtDir / names.componentSingletonType

      val userImplFile = extRoot / s"${names.componentSingletonType}.c"
      val userHeaderFile = extRoot / s"${names.componentSingletonType}.h"

      val apiFilename = NixSeL4NameUtil.apiHelperFilename(names)
      val apiHeaderFile = extRoot / s"${apiFilename}.h"
      val apiImplFile = extRoot / s"${apiFilename}.c"

      var entrypointAdapters: ISZ[ST] = ISZ()

      val logInfo = NixSeL4NameUtil.apiHelperMethodName("logInfo", names)

      { // add entrypoint stubs
        var entrypointSignatures: ISZ[ST] = ISZ()

        val params: ISZ[ST] = ISZ()

        val (initMethodSig, initMethodImpl, initAdapterMethod) = genStubInitializeMethod(names, ports, apiImplFile.name, userImplFile.name)
        val (finalizeMethodSig, finalizeMethodImpl, finalizeAdapterMethod) = genStubFinaliseMethod(names, apiImplFile.name, userImplFile.name)

        entrypointAdapters = entrypointAdapters :+ initAdapterMethod
        entrypointAdapters = entrypointAdapters :+ finalizeAdapterMethod

        entrypointSignatures = (entrypointSignatures :+ initMethodSig) :+ finalizeMethodSig

        var methods: ISZ[ST] = ISZ(initMethodImpl, finalizeMethodImpl)

        var exampleApiUsage: ISZ[ST] = ISZ()
        var tindex = z"0"

        for(p <- ports.filter(f => CommonUtil.isInPort(f.feature))) {
          val getter = NixSeL4NameUtil.apiHelperGetterMethodName(p.name, names)
          val str = s"${p.name}_str"
          val s: ST = if(CommonUtil.isDataPort(p.feature)) {
            val t = s"t$tindex"
            tindex = tindex + 1

            val entry:ST = {
              if(types.rawConnections) {
                val originatingTypeNames: DataTypeNames = p._portType match {
                  case BitType(_, _, _, Some(o)) => Util.getDataTypeNames(o, names.basePackage)
                  case _ => halt(s"Unexpected: Could not find originating type for ${p._portType}")
                }

                val numBits = BitCodecNameUtil.numBitsConstName(originatingTypeNames.qualifiedCTypeName)
                val numBytes = BitCodecNameUtil.numBytesConstName(originatingTypeNames.qualifiedCTypeName)
                val bitsName = s"${t}_numBits"

                st"""uint8_t ${t}[${numBytes}];
                    |size_t ${bitsName};
                    |if(${getter}(${StackFrameTemplate.SF} &${bitsName}, ${t})) {
                    |  // sanity check
                    |  sfAssert(${StackFrameTemplate.SF} (Z) ${bitsName} == ${numBits}, "numBits received does not match expected");
                    |
                    |  DeclNewString(${str});
                    |  String__append(${StackFrameTemplate.SF} (String) &${str}, string("Received on ${p.name}: "));
                    |  byte_array_string(${StackFrameTemplate.SF} (String) &${str}, ${t}, ${numBytes});
                    |  ${logInfo}(${StackFrameTemplate.SF} (String) &${str});
                    |}"""
              }
              else {
                val (refName, decl): (String, ST) = if (p.getPortTypeNames.isEnum() || p.getPortTypeNames.isBaseType()) {
                  (t, st"${p.getPortTypeNames.qualifiedCTypeName} $t;")
                } else {
                  (s"&$t", st"DeclNew${p.getPortTypeNames.qualifiedCTypeName}($t);")
                }
                st"""${decl}
                    |if(${getter}(${StackFrameTemplate.SF} &${t})) {
                    |  DeclNewString(${str});
                    |  String__append(${StackFrameTemplate.SF} (String) &${str}, string("Received on ${p.name}: "));
                    |  ${p.getPortTypeNames.qualifiedCTypeName}_string_(${StackFrameTemplate.SF} (String) &${str}, ${refName});
                    |  ${logInfo}(${StackFrameTemplate.SF} (String) &${str});
                    |}"""
              }
            }
            entry
          } else {
            st"""if(${getter}(${StackFrameTemplate.SF_LAST} )){
                |  String ${str} = string("Received event on ${p.name}");
                |  ${logInfo}(${StackFrameTemplate.SF} ${str});
                |}"""
          }
          exampleApiUsage = exampleApiUsage :+ s
        }

        val _exampleApiUsage: Option[ST] =
          if(exampleApiUsage.isEmpty) None()
          else Some(st"""// examples of api getter usage
                        |
                        |${(exampleApiUsage, "\n\n")}""")

        PropertyUtil.getDispatchProtocol(c) match {
          case Some(Dispatch_Protocol.Periodic) =>
            // timetriggered
            val apiMethodName = s"${componentName}_timeTriggered"
            val userMethodName = s"${apiMethodName}_"
            val timeTriggeredSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")
            entrypointSignatures = entrypointSignatures :+ timeTriggeredSig
            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = userImplFile.name,
              owner = "",
              name = userMethodName,
              line = 0)

            methods = methods :+
              st"""${timeTriggeredSig} {
                  |  ${declNewStackFrame};
                  |
                  |  ${_exampleApiUsage}
                  |}"""

            val api_params = ISZ(st"${names.cOperationalApi} api")
            val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
            val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = apiMethodName,
              line = 0)
            val apiAdapterMethod: ST =
              st"""${apiMethodSig} {
                  |  ${apiDeclNewStackFrame};
                  |
                  |  ${userMethodName}(${StackFrameTemplate.SF_LAST});
                  |}"""

            entrypointAdapters = entrypointAdapters :+ apiAdapterMethod

          case Some(Dispatch_Protocol.Sporadic) =>
            val inEventPorts = ports.filter(f => CommonUtil.isEventPort(f.feature) && CommonUtil.isInFeature(f.feature))

            var dumpedExampleGetterApiUsageAlready: B = F
            for (p <- inEventPorts) {
              val isEventData: B = p.feature.category == ir.FeatureCategory.EventDataPort
              val handlerName = s"${componentName}_handle_${p.name}"
              val apiMethodName = handlerName
              val handlerMethodName = s"${handlerName}_"

              var eventDataParams: ISZ[ST] = params
              if (isEventData) {
                val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handlerSig = SeL4NixTemplate.methodSignature(handlerMethodName, eventDataParams, "Unit")
              entrypointSignatures = entrypointSignatures :+ handlerSig

              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = userImplFile.name,
                owner = "",
                name = handlerMethodName,
                line = 0)

              if (types.rawConnections && p.feature.category == ir.FeatureCategory.EventDataPort) {
                val rawHandlerMethodName = s"${handlerName}_raw"
                val numBits = "numBits"
                val byteArray = "byteArray"
                val rawParams: ISZ[ST] = params :+ st"size_t ${numBits}" :+ st"uint8_t *${byteArray}"

                val rawHandler = SeL4NixTemplate.methodSignature(rawHandlerMethodName, rawParams, "Unit")

                val declNewStackFrameRaw: ST = StackFrameTemplate.DeclNewStackFrame(
                  caller = T,
                  uri = apiImplFile.name,
                  owner = "",
                  name = rawHandlerMethodName,
                  line = 0)

                val str = s"${p.name}String"
                methods = methods :+
                  st"""${rawHandler} {
                      |  ${declNewStackFrameRaw};
                      |
                      |  size_t numBytes = ${numBits} == 0 ? 0 : (${numBits} - 1) / 8 + 1;
                      |  DeclNewString(${p.name}String);
                      |  String__append(${StackFrameTemplate.SF} (String) &${str}, string("${rawHandlerMethodName} called"));
                      |  byte_array_string(${StackFrameTemplate.SF} (String) &${str}, ${byteArray}, numBytes);
                      |  ${logInfo} (${StackFrameTemplate.SF} (String) &${str});
                      |}"""

                val __exampleApiUsage: Option[ST] =
                  if(!dumpedExampleGetterApiUsageAlready && _exampleApiUsage.nonEmpty) {
                    dumpedExampleGetterApiUsageAlready = T
                    _exampleApiUsage
                  } else {
                    None()
                  }

                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  ${rawHandlerMethodName}(${StackFrameTemplate.SF} value->size, value->value);
                      |
                      |  ${__exampleApiUsage}
                      |}"""
              }
              else {
                val printValue: ST = if(isEventData) {
                  st"""DeclNewString(_str);
                      |String__append(${StackFrameTemplate.SF} (String) &_str, string("Received on ${p.name}: "));
                      |${p.getPortTypeNames.qualifiedCTypeName}_string_(${StackFrameTemplate.SF} (String) &_str, value);
                      |${logInfo}(${StackFrameTemplate.SF} (String) &_str);"""
                } else {
                  st"""String str = string("Received event on ${p.name}");
                      |${logInfo}(${StackFrameTemplate.SF} str);"""
                }

                val __exampleApiUsage: Option[ST] =
                  if(!dumpedExampleGetterApiUsageAlready && _exampleApiUsage.nonEmpty) {
                    dumpedExampleGetterApiUsageAlready = T
                    _exampleApiUsage
                  } else {
                    None()
                  }

                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  DeclNewString(${p.name}String);
                      |  String__append(${StackFrameTemplate.SF} (String) &${p.name}String, string("${handlerName} called"));
                      |  ${logInfo} (${StackFrameTemplate.SF} (String) &${p.name}String);
                      |
                      |  ${printValue}
                      |
                      |  ${__exampleApiUsage}
                      |}"""
              }

              val api_params = ISZ(st"${names.cOperationalApi} api") ++ eventDataParams
              val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
              val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = apiMethodName,
                line = 0)

              val valueArg: String = if(isEventData) s"${StackFrameTemplate.SF} value" else StackFrameTemplate.SF_LAST

              val apiAdapterMethod: ST =
                st"""${apiMethodSig} {
                    |  ${apiDeclNewStackFrame};
                    |
                    |  ${handlerMethodName}(${valueArg});
                    |}"""

              entrypointAdapters = entrypointAdapters :+ apiAdapterMethod
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        {
          extensionFiles = extensionFiles :+ userHeaderFile
          val userHeaderMethods = ops.ISZOps(entrypointSignatures).map((s: ST) => st"${s};")
          val userMacroName = StringUtil.toUpperCase(s"${names.componentSingletonType}_h")
          val headerSt = SeL4NixTemplate.cHeaderFile(userMacroName, userHeaderMethods)
          resources = resources :+ ResourceUtil.createResource(
            Util.pathAppend(userHeaderFile.up.value, ISZ(userHeaderFile.name)), headerSt, T)
        }

        val impl =
          st"""#include <${apiHeaderFile.name}>
              |#include <${userHeaderFile.name}>
              |#include <${NixGen.EXT_H}>
              |
              |${StringTemplate.safeToEditComment()}
              |
              |${(methods, "\n\n")}
              |"""

        {
          val implFile = extRoot / s"${names.componentSingletonType}.c"
          extensionFiles = extensionFiles :+ implFile
          resources = resources :+ ResourceUtil.createResource(
            Util.pathAppend(implFile.up.value, ISZ(implFile.name)), impl, F)
        }
      }

      { // api helper methods
        var headerMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")
        var implMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")

        for (p <- ports) {
          val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)

          p.feature.direction match {
            case ir.Direction.In => {

              val cApiMethodName = NixSeL4NameUtil.apiHelperGetterMethodName(p.name, names)
              val returnType = "bool"
              val slangApiGetMethodName = s"${names.cOperationalApi}_get_${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"size_t *numBits",
                  st"uint8_t *byteArray")

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet_byteArrayVersion(
                  names = names,
                  signature = altSignature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  typ = typeNames)
              } else {
                val pointer: String = if (typeNames.isEnum() || typeNames.isBaseType()) "*" else ""

                var params: ISZ[ST] = ISZ()
                if (!typeNames.isEmptyType()) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"
                }

                val signature = SeL4NixTemplate.methodSignature(cApiMethodName, params, returnType)

                headerMethods = headerMethods :+ st"${signature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet(
                  names = names,
                  signature = signature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  typ = typeNames)
              }
            }
            case ir.Direction.Out => {
              val isEventPort = p.feature.category == ir.FeatureCategory.EventPort

              val cApiMethodName = NixSeL4NameUtil.apiHelperSetterMethodName(p.name, names)
              val returnType = "void"
              val slangApiSetMethodName = s"${names.cInitializationApi}_put_${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"size_t numBits",
                  st"uint8_t *byteArray"
                )
                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet_byteArrayVersion(
                  names,
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName)
              } else {

                var params: ISZ[ST] = ISZ()
                if (!isEventPort) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} value"
                }

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, params, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet(
                  names,
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName,
                  isEventPort)
              }
            }
            case x => halt(s"Unexpected direction ${x}")
          }
        }

        { // logging methods

          val loggers = ISZ("logInfo", "logDebug", "logError")

          for (l <- loggers) {
            val methodName = NixSeL4NameUtil.apiHelperMethodName(l, names)
            val params = ISZ(st"String str")

            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = methodName,
              line = 0)

            val signature = SeL4NixTemplate.methodSignature(methodName, params, "void")

            val apiLogMethodName = s"${names.cInitializationApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ SeL4NixTemplate.apiLog(names, signature, declNewStackFrame, apiLogMethodName)
          }
        }

        implMethods = implMethods ++ entrypointAdapters

        val macroName = StringUtil.toUpperCase(s"${apiFilename}_h")

        val headerContents = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

        val implContents = SeL4NixTemplate.cImplFile(apiFilename, implMethods, ISZ(s"<${userHeaderFile.name}>"))

        extensionFiles = extensionFiles :+ apiHeaderFile
        extensionFiles = extensionFiles :+ apiImplFile

        resources = resources :+ ResourceUtil.createResource(Util.pathAppend(apiHeaderFile.up.value, ISZ(apiHeaderFile.name)), headerContents, T)
        resources = resources :+ ResourceUtil.createResource(Util.pathAppend(apiImplFile.up.value, ISZ(apiImplFile.name)), implContents, T)
      } // end helper api methods
    }

    return (extensionFiles, resources)
  }

  def genStubInitializeMethod(names: Names, ports: ISZ[Port], apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_initialise"
    val userMethodName = s"${apiMethodName}_"
    val initialiseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")

    var statements: ISZ[ST] = ISZ()

    var resultCount = z"0"
    for(p <- ports.filter(f => CommonUtil.isOutPort(f.feature))) {
      val setterName = NixSeL4NameUtil.apiHelperSetterMethodName(p.name, names)
      val u: ST = if(CommonUtil.isDataPort(p.feature)) {
        val result = s"t${resultCount}"
        resultCount = resultCount + 1
        val decl: ST =
          if(types.rawConnections) {
            val originatingTypeNames: DataTypeNames = p._portType match {
              case BitType(_, _, _, Some(o)) => Util.getDataTypeNames(o, names.basePackage)
              case _ =>halt(s"Unexpected: Could not find originating type for ${p._portType}")
            }

            val numBits = BitCodecNameUtil.numBitsConstName(originatingTypeNames.qualifiedCTypeName)
            val numBytes = BitCodecNameUtil.numBytesConstName(originatingTypeNames.qualifiedCTypeName)

            st"""uint8_t ${result}[${numBytes}];
                |byte_array_default(${StackFrameTemplate.SF} ${result}, ${numBits}, ${numBytes});
                |${setterName}(${StackFrameTemplate.SF} ${numBits}, ${result});"""

          } else {
            if (p.getPortTypeNames.isEnum()) {
              st"""${p.getPortTypeNames.qualifiedCTypeName} ${result} = ${p.getPortTypeNames.example_C_Name()};
                  |${setterName}(${StackFrameTemplate.SF} ${result});"""
            } else if(p.getPortTypeNames.isBaseType()){
              st"""${p.getPortTypeNames.qualifiedCTypeName} ${result} = ${p.getPortTypeNames.example_C_Name()}(${StackFrameTemplate.SF_LAST});
                  |${setterName}(${StackFrameTemplate.SF} ${result});"""
            } else {
              st"""DeclNew${p.getPortTypeNames.qualifiedCTypeName}(${result});
                  |${p.getPortTypeNames.example_C_Name()}(${StackFrameTemplate.SF} &${result});
                  |${setterName}(${StackFrameTemplate.SF} &${result});"""
            }
          }
        decl
      } else {
        st"${setterName}(${StackFrameTemplate.SF_LAST});"
      }

      statements = statements :+ u
    }

    val loggers: ISZ[ST] = ISZ[String]("logInfo", "logDebug", "logError").map((l: String) => {
      val mname = NixSeL4NameUtil.apiHelperMethodName(l, names)
      st"""${mname}(${StackFrameTemplate.SF} string("Example ${l}"));"""
    })

    statements = statements ++ loggers

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val initMethodImpl: ST =
      st"""${initialiseMethodSig} {
          |  ${declNewStackFrame};
          |
          |  // examples of api setter and logging usage
          |
          |  ${(statements, "\n\n")}
          |}"""

    val api_params = ISZ(st"${names.cInitializationApi} api")
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
    val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = apiFileUri,
      owner = "",
      name = apiMethodName,
      line = 0)
    val apiMethodImpl: ST =
      st"""${apiMethodSig} {
          |  ${apiDeclNewStackFrame};
          |
          |  ${userMethodName}(${StackFrameTemplate.SF_LAST});
          |}"""
    return (initialiseMethodSig, initMethodImpl, apiMethodImpl)
  }

  def genStubFinaliseMethod(names: Names, apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_finalise"
    val userMethodName = s"${apiMethodName}_"
    val finaliseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val ret: ST =
      st"""${finaliseMethodSig} {
          |  ${declNewStackFrame};
          |}"""

    val api_params = ISZ(st"${names.cOperationalApi} api")
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
    val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = apiFileUri,
      owner = "",
      name = apiMethodName,
      line = 0)
    val adapterMethod: ST =
      st"""${apiMethodSig} {
          |  ${apiDeclNewStackFrame};
          |
          |  ${userMethodName}(${StackFrameTemplate.SF_LAST});
          |}"""
    return (finaliseMethodSig, ret, adapterMethod)
  }

  def getExistingCFiles(cExtensionDir: String): ISZ[Os.Path] = {
    val p = Os.path(cExtensionDir)
    val ret: ISZ[Os.Path] = if (p.exists && p.isDir) {
      p.list.filter(f => f.ext == "c" || f.ext == "h")
        .filter(f => !ops.ISZOps(NixGen.KNOWN_HAMR_PROVIDED_FILES).contains(f.name))
    } else {
      ISZ()
    }
    return ret
  }

  def genTypeTouches(types: AadlTypes, basePackage: String): ISZ[ST] = {
    var a: ISZ[ST] = ISZ()
    var counter: Z = z"0"
    val _types: ISZ[AadlType] = if (types.rawConnections) {
      // TODO all types in typesMap should be BitTypes that optionally link
      // back to their originating type
      ISZ(BitType(TypeUtil.SlangEmbeddedBitTypeName, None(), None(), None()))
    } else {
      types.typeMap.entries.map((x: (String, AadlType)) => x._2)
    }

    for (typ <- _types) {
      val typeNames: DataTypeNames = Util.getDataTypeNames(typ, basePackage)
      a = a :+ SeL4NixTemplate.touchType(typeNames.qualifiedPayloadName, Some(typeNames.example()))
      counter = counter + z"1"
    }
    a = a :+ SeL4NixTemplate.touchType("art.Empty", None())
    return a
  }

}

object NixGenDispatch {

  def generate(dirs: ProjectDirectories,
               root: AadlSystem,
               arsitOptions: ArsitOptions,
               symbolTable: SymbolTable,
               types: AadlTypes,
               previousPhase: Result): ArsitResult = {

    val ret: ArsitResult = arsitOptions.platform match {
      case ArsitPlatform.Linux =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.MacOS =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case _ =>
        ArsitResult(
          previousPhase.resources,
          previousPhase.maxPort,
          previousPhase.maxComponent,
          ISZ()
        )
    }
    return ret
  }
}
