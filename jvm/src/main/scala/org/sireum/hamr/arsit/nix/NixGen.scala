// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{SeL4NixTemplate, StringTemplate}
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, DataTypeNames, TypeUtil}
import org.sireum.hamr.codegen.common.{CommonUtil, Names, SeL4NixNamesUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object NixGen{
  val IPC_C: String = "ipc.c"
  val EXT_H: String = "ext.h"
  val EXT_C: String = "ext.c"

  val KNOWN_HAMR_PROVIDED_FILES: ISZ[String] = ISZ(IPC_C, EXT_H, EXT_C)
}

@msig trait NixGen {
  def dirs: ProjectDirectories

  def cExtensionDir: String

  def root: AadlSystem

  def arsitOptions: ArsitOptions

  def symbolTable: SymbolTable

  def types: AadlTypes

  def previousPhase: Result

  def reporter: Reporter

  def generate(): ArsitResult

  def genExtensionFiles(c: ir.Component, names: Names, ports: ISZ[Port]): (ISZ[Os.Path], ISZ[Resource]) = {

    val rootExtDir = Os.path(cExtensionDir)

    var extensionFiles: ISZ[Os.Path] = ISZ()
    var resources: ISZ[Resource] = ISZ()

    val extC = rootExtDir / NixGen.EXT_C
    val extH = rootExtDir / NixGen.EXT_H
    resources = resources :+ Util.createResource(extC.up.value, ISZ(extC.name), Util.getLibraryFile(NixGen.EXT_C), F)
    resources = resources :+ Util.createResource(extH.up.value, ISZ(extH.name), Util.getLibraryFile(NixGen.EXT_H), F)

    extensionFiles = (extensionFiles :+ extC) :+ extH

    if (arsitOptions.excludeImpl) {

      val componentName = names.cComponentType
      val extRoot = rootExtDir / names.componentSingletonType

      val userImplFile = extRoot / s"${names.componentSingletonType}.c"
      val userHeaderFile = extRoot / s"${names.componentSingletonType}.h"

      val apiFilename = SeL4NixNamesUtil.apiHelperFilename(names)
      val apiHeaderFile = extRoot / s"${apiFilename}.h"
      val apiImplFile = extRoot / s"${apiFilename}.c"
      val preParams: Option[ST] = Some(StackFrameTemplate.STACK_FRAME_ST)

      var entrypointAdapters: ISZ[ST] = ISZ()

      { // add entrypoint stubs
        var entrypointSignatures: ISZ[ST] = ISZ()

        val params: ISZ[ST] = ISZ()

        val (initMethodSig, initMethodImpl, initAdapterMethod) = genStubInitializeMethod(names, ports, apiImplFile.name, userImplFile.name)
        val (finalizeMethodSig, finalizeMethodImpl, finalizeAdapterMethod) = genStubFinaliseMethod(names, apiImplFile.name, userImplFile.name)

        entrypointAdapters = entrypointAdapters :+ initAdapterMethod
        entrypointAdapters = entrypointAdapters :+ finalizeAdapterMethod

        entrypointSignatures = (entrypointSignatures :+ initMethodSig) :+ finalizeMethodSig

        var methods: ISZ[ST] = ISZ(initMethodImpl, finalizeMethodImpl)

        PropertyUtil.getDispatchProtocol(c) match {
          case Some(Dispatch_Protocol.Periodic) =>
            // timetriggered
            val apiMethodName = s"${componentName}_timeTriggered"
            val userMethodName = s"${apiMethodName}_"
            val timeTriggeredSig = SeL4NixTemplate.methodSignature(userMethodName, preParams, params, "Unit")
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
                  |}"""

            val api_params = ISZ(st"${names.cOperationalApi} api")
            val api_pre_params = Some(StackFrameTemplate.STACK_FRAME_ST)
            val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_pre_params, api_params, "Unit")
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

            for (p <- inEventPorts) {
              val isEventData: B = p.feature.category == ir.FeatureCategory.EventDataPort
              val handlerName = s"${componentName}_handle${p.name}"
              val apiMethodName = handlerName
              val handlerMethodName = s"${handlerName}_"

              var eventDataParams: ISZ[ST] = params
              if (isEventData) {
                val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handlerSig = SeL4NixTemplate.methodSignature(handlerMethodName, preParams, eventDataParams, "Unit")
              entrypointSignatures = entrypointSignatures :+ handlerSig
              val logInfo = SeL4NixNamesUtil.apiHelperMethodName("logInfo", names);

              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = userImplFile.name,
                owner = "",
                name = handlerMethodName,
                line = 0)

              if (types.rawConnections && p.feature.category == ir.FeatureCategory.EventDataPort) {
                val rawHandlerMethodName = s"${handlerName}_raw"

                val rawParams: ISZ[ST] = params :+ st"size_t numBits" :+ st"uint8_t *byteArray"

                val rawHandler = SeL4NixTemplate.methodSignature(rawHandlerMethodName, preParams, rawParams, "Unit")

                val declNewStackFrameRaw: ST = StackFrameTemplate.DeclNewStackFrame(
                  caller = T,
                  uri = apiImplFile.name,
                  owner = "",
                  name = rawHandlerMethodName,
                  line = 0)

                methods = methods :+
                  st"""${rawHandler} {
                      |  ${declNewStackFrameRaw};
                      |
                      |  DeclNewString(${p.name}String);
                      |  String__append(${StackFrameTemplate.SF} (String) &${p.name}String, string("${rawHandlerMethodName} called"));
                      |  ${logInfo} (${StackFrameTemplate.SF} (String) &${p.name}String);
                      |}"""

                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  ${rawHandlerMethodName}(${StackFrameTemplate.SF} value->size, value->value);
                      |}"""
              }
              else {
                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  DeclNewString(${p.name}String);
                      |  String__append(${StackFrameTemplate.SF} (String) &${p.name}String, string("${handlerName} called"));
                      |  ${logInfo} (${StackFrameTemplate.SF} (String) &${p.name}String);
                      |}"""
              }

              val api_params = ISZ(st"${names.cOperationalApi} api") ++ eventDataParams
              val api_pre_params = Some(StackFrameTemplate.STACK_FRAME_ST)
              val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_pre_params, api_params, "Unit")
              val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = apiMethodName,
                line = 0)

              val valueArg: String = if(isEventData) " value" else ""

              val apiAdapterMethod: ST =
                st"""${apiMethodSig} {
                    |  ${apiDeclNewStackFrame};
                    |
                    |  ${handlerMethodName}(${StackFrameTemplate.SF_LAST}${valueArg});
                    |}"""

              entrypointAdapters = entrypointAdapters :+ apiAdapterMethod
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        {
          extensionFiles = extensionFiles :+ userHeaderFile
          val userHeaderMethods = ops.ISZOps(entrypointSignatures).map(s => st"${s};")
          val userMacroName = StringUtil.toUpperCase(s"${names.componentSingletonType}_h")
          val headerSt = SeL4NixTemplate.cHeaderFile(userMacroName, userHeaderMethods)
          resources = resources :+ Util.createResource(userHeaderFile.up.value, ISZ(userHeaderFile.name), headerSt, T)
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
          resources = resources :+ Util.createResource(implFile.up.value, ISZ(implFile.name), impl, F)
        }
      }

      { // api helper methods
        var headerMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")
        var implMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")

        for (p <- ports) {
          val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)

          p.feature.direction match {
            case ir.Direction.In => {

              val cApiMethodName = SeL4NixNamesUtil.apiHelperGetterMethodName(p.name, names)
              val returnType = "bool"
              val slangApiGetMethodName = s"${names.cOperationalApi}_get${p.name}_"
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

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, altParams, returnType)

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

                val signature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, params, returnType)

                headerMethods = headerMethods :+ st"${signature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet(
                  names = names,
                  signature = signature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  api = names.cOperationalApi_Id,
                  typ = typeNames)
              }
            }
            case ir.Direction.Out => {
              val isEventPort = p.feature.category == ir.FeatureCategory.EventPort

              val cApiMethodName = SeL4NixNamesUtil.apiHelperSetterMethodName(p.name, names)
              val returnType = "void"
              val sendSet: String = if (CommonUtil.isAadlDataPort(p.feature)) "set" else "send"
              val slangApiSetMethodName = s"${names.cInitializationApi}_${sendSet}${p.name}_"
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
                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, altParams, returnType)

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

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, params, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet(
                  names,
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName,
                  names.cInitializationApi_Id,
                  isEventPort)
              }
            }
            case x => halt(s"Unexpected direction ${x}")
          }
        }

        { // logging methods

          val loggers = ISZ("logInfo", "logDebug", "logError")

          for (l <- loggers) {
            val methodName = SeL4NixNamesUtil.apiHelperMethodName(l, names)
            val params = ISZ(st"String str")

            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = methodName,
              line = 0)

            val signature = SeL4NixTemplate.methodSignature(methodName, preParams, params, "void")

            val apiLogMethodName = s"${names.cInitializationApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ SeL4NixTemplate.apiLog(names, signature, declNewStackFrame, apiLogMethodName, names.cInitializationApi_Id)
          }
        }

        implMethods = implMethods ++ entrypointAdapters

        val macroName = StringUtil.toUpperCase(s"${apiFilename}_h")

        val headerContents = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

        val implContents = SeL4NixTemplate.cImplFile(apiFilename, implMethods, ISZ(s"<${userHeaderFile.name}>"))

        extensionFiles = extensionFiles :+ apiHeaderFile
        extensionFiles = extensionFiles :+ apiImplFile

        resources = resources :+ Util.createResource(apiHeaderFile.up.value, ISZ(apiHeaderFile.name), headerContents, T)
        resources = resources :+ Util.createResource(apiImplFile.up.value, ISZ(apiImplFile.name), implContents, T)
      } // end helper api methods
    }

    return (extensionFiles, resources)
  }

  def genStubInitializeMethod(names: Names, ports: ISZ[Port], apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val preParams = Some(StackFrameTemplate.STACK_FRAME_ONLY_ST)
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_initialise"
    val userMethodName = s"${apiMethodName}_"
    val initialiseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, preParams, params, "Unit")

    val loggers: ISZ[String] = ISZ("logInfo", "logDebug", "logError")
    val statements: ISZ[ST] = loggers.map((l: String) => {
      val mname = SeL4NixNamesUtil.apiHelperMethodName(l, names)
      st"""${mname}(${StackFrameTemplate.SF} string("Example ${l}"));"""
    })
    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val ret: ST =
      st"""${initialiseMethodSig} {
          | ${declNewStackFrame};
          |
          | // example api usage
          |
          | ${(statements, "\n")}
          |}"""

    val api_params = ISZ(st"${names.cInitializationApi} api")
    val api_pre_params = Some(StackFrameTemplate.STACK_FRAME_ST)
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_pre_params, api_params, "Unit")
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
    return (initialiseMethodSig, ret, adapterMethod)
  }

  def genStubFinaliseMethod(names: Names, apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val preParams = Some(StackFrameTemplate.STACK_FRAME_ONLY_ST)
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_finalise"
    val userMethodName = s"${apiMethodName}_"
    val finaliseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, preParams, params, "Unit")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val ret: ST =
      st"""${finaliseMethodSig} {
          |  ${declNewStackFrame};
          |
          |  // example finalise method
          |}"""

    val api_params = ISZ(st"${names.cOperationalApi} api")
    val api_pre_params = Some(StackFrameTemplate.STACK_FRAME_ST)
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_pre_params, api_params, "Unit")
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
      ISZ(TypeUtil.SlangEmbeddedBitType)
    } else {
      types.typeMap.entries.map((x: (String, AadlType)) => x._2)
    }

    for (typ <- _types) {
      //val typ = t._2
      val typeNames: DataTypeNames = Util.getDataTypeNames(typ, basePackage)
      a = a :+ SeL4NixTemplate.touchType(typeNames.qualifiedPayloadName, Some(typeNames.empty()))
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
               reporter: Reporter,
               previousPhase: Result): ArsitResult = {

    val cExtensionDir: String = if (arsitOptions.auxCodeDir.nonEmpty) {
      assert(arsitOptions.auxCodeDir.size == 1)
      arsitOptions.auxCodeDir(0)
    } else {
      Util.pathAppend(dirs.srcDir, ISZ("c", "ext-c"))
    }

    val ret: ArsitResult = arsitOptions.platform match {
      case ArsitPlatform.Linux =>
        ArtNixGen(dirs, cExtensionDir, root, arsitOptions, symbolTable, types, previousPhase, reporter).generate()
      case ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, cExtensionDir, root, arsitOptions, symbolTable, types, previousPhase, reporter).generate()
      case ArsitPlatform.MacOS =>
        ArtNixGen(dirs, cExtensionDir, root, arsitOptions, symbolTable, types, previousPhase, reporter).generate()
      case ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, cExtensionDir, root, arsitOptions, symbolTable, types, previousPhase, reporter).generate()
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
