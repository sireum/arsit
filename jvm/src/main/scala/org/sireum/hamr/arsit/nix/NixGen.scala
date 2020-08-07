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
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.{CommonUtil, Names, SeL4NixNamesUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.message.Reporter

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

    val extC = rootExtDir / "ext.c"
    val extH = rootExtDir / "ext.h"
    resources = resources :+ Util.createResource(extC.up.value, ISZ(extC.name), Util.getLibraryFile("ext.c"), F)
    resources = resources :+ Util.createResource(extH.up.value, ISZ(extH.name), Util.getLibraryFile("ext.h"), F)

    extensionFiles = (extensionFiles :+ extC) :+ extH

    if (arsitOptions.excludeImpl) {

      val componentName = names.cComponentImpl
      val extRoot = rootExtDir / names.componentImpl
      val apiFilename = SeL4NixNamesUtil.apiHelperFilename(names)
      val apiHeaderFile = extRoot / s"${apiFilename}.h"
      val apiImplFile = extRoot / s"${apiFilename}.c"
      val preParams: Option[ST] = Some(StackFrameTemplate.STACK_FRAME_ST)

      { // api helper methods
        var headerMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")
        var implMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")

        for (p <- ports) {
          val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)

          p.feature.direction match {
            case ir.Direction.In => {

              val cApiMethodName = SeL4NixNamesUtil.apiHelperGetterMethodName(p.name, names)
              val returnType = "bool"
              val slangApiGetMethodName = s"${names.cBridgeApi}_get${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"${componentName} this",
                  st"size_t *numBits",
                  st"uint8_t *byteArray")

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet_byteArrayVersion(
                  signature = altSignature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  c_this = names.cThisApi,
                  typ = typeNames)
              } else {
                val pointer: String = if (typeNames.isEnum() || typeNames.isBaseType()) "*" else ""

                var params = ISZ(st"${componentName} this")
                if (!typeNames.isEmptyType()) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"
                }

                val signature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, params, returnType)

                headerMethods = headerMethods :+ st"${signature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet(
                  signature = signature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  c_this = names.cThisApi,
                  typ = typeNames)
              }
            }
            case ir.Direction.Out => {
              val isEventPort = p.feature.category == ir.FeatureCategory.EventPort

              val cApiMethodName = SeL4NixNamesUtil.apiHelperSetterMethodName(p.name, names)
              val returnType = "void"
              val sendSet: String = if (CommonUtil.isAadlDataPort(p.feature)) "set" else "send"
              val slangApiSetMethodName = s"${names.cBridgeApi}_${sendSet}${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"${componentName} this",
                  st"size_t numBits",
                  st"uint8_t *byteArray"
                )
                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet_byteArrayVersion(
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName,
                  names.cThisApi)
              } else {

                var params = ISZ(st"${componentName} this")
                if (!isEventPort) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} value"
                }

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, preParams, params, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet(
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName,
                  names.cThisApi,
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
            val params = ISZ(st"${componentName} this", st"String str")

            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = methodName,
              line = 0)

            val signature = SeL4NixTemplate.methodSignature(methodName, preParams, params, "void")

            val apiLogMethodName = s"${names.cBridgeApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ SeL4NixTemplate.apiLog(signature, declNewStackFrame, apiLogMethodName, names.cThisApi)
          }
        }

        val macroName = StringUtil.toUpperCase(s"${apiFilename}_h")

        val headerContents = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

        val implContents = SeL4NixTemplate.cImplFile(apiFilename, implMethods)

        extensionFiles = extensionFiles :+ apiHeaderFile
        extensionFiles = extensionFiles :+ apiImplFile

        resources = resources :+ Util.createResource(apiHeaderFile.up.value, ISZ(apiHeaderFile.name), headerContents, T)
        resources = resources :+ Util.createResource(apiImplFile.up.value, ISZ(apiImplFile.name), implContents, T)
      } // end helper api methods


      { // add entrypoint stubs

        val params: ISZ[ST] = ISZ(st"${componentName} this")

        var methods: ISZ[ST] = ISZ(
          genStubInitializeMethod(names, ports, apiImplFile.name),
          genStubFinaliseMethod(names, apiImplFile.name))

        PropertyUtil.getDispatchProtocol(c) match {
          case Some(Dispatch_Protocol.Periodic) =>
            // timetriggered
            val methodName = s"${componentName}_timeTriggered_"
            val timeTriggered = SeL4NixTemplate.methodSignature(methodName, preParams, params, "Unit")
            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = methodName,
              line = 0)
            methods = methods :+
              st"""${timeTriggered} {
                  |  ${declNewStackFrame};
                  |}"""
          case Some(Dispatch_Protocol.Sporadic) =>
            val inEventPorts = ports.filter(f => CommonUtil.isEventPort(f.feature) && CommonUtil.isInFeature(f.feature))

            for (p <- inEventPorts) {
              val handlerName = s"${componentName}_handle${p.name}"
              val handlerMethodName = s"${handlerName}_"
              var eventDataParams = params
              if (p.feature.category == ir.FeatureCategory.EventDataPort) {
                val typeNames = Util.getDataTypeNames(p._portType, names.basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handler = SeL4NixTemplate.methodSignature(handlerMethodName, preParams, eventDataParams, "Unit")
              val logInfo = SeL4NixNamesUtil.apiHelperMethodName("logInfo", names);

              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
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
                      |  ${logInfo} (${StackFrameTemplate.SF} this, (String) &${p.name}String);
                      |}"""

                methods = methods :+
                  st"""${handler} {
                      |  ${declNewStackFrame};
                      |
                      |  ${rawHandlerMethodName}(${StackFrameTemplate.SF} this, value->size, value->value);
                      |}"""
              }
              else {
                methods = methods :+
                  st"""${handler} {
                      |  ${declNewStackFrame};
                      |
                      |  DeclNewString(${p.name}String);
                      |  String__append(${StackFrameTemplate.SF} (String) &${p.name}String, string("${handlerName} called"));
                      |  ${logInfo} (${StackFrameTemplate.SF} this, (String) &${p.name}String);
                      |}"""
              }
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        val impl =
          st"""#include <${apiHeaderFile.name}>
              |#include <ext.h>
              |
              |${StringTemplate.safeToEditComment()}
              |
              |${(methods, "\n\n")}
              |"""

        val implFile = extRoot / s"${names.componentImpl}.c"
        extensionFiles = extensionFiles :+ implFile
        resources = resources :+ Util.createResource(implFile.up.value, ISZ(implFile.name), impl, F)
      }
    }

    return (extensionFiles, resources)
  }

  def genStubInitializeMethod(names: Names, ports: ISZ[Port], fileUri: String): ST = {
    val preParams = Some(StackFrameTemplate.STACK_FRAME_ST)
    val params: ISZ[ST] = ISZ(st"${names.cComponentImpl} this")
    val methodName = s"${names.cComponentImpl}_initialise_"
    val initialiseMethod = SeL4NixTemplate.methodSignature(methodName, preParams, params, "Unit")

    val loggers: ISZ[String] = ISZ("logInfo", "logDebug", "logError")
    val statements: ISZ[ST] = loggers.map((l: String) => {
      val mname = SeL4NixNamesUtil.apiHelperMethodName(l, names)
      st"""${mname}(${StackFrameTemplate.SF} this, string("Example ${l}"));"""
    })
    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = fileUri,
      owner = "",
      name = methodName,
      line = 0)

    val ret: ST =
      st"""${initialiseMethod} {
          | ${declNewStackFrame};
          |
          | // example api usage
          |
          | ${(statements, "\n")}
          |}"""
    return ret
  }

  def genStubFinaliseMethod(names: Names, fileUri: String): ST = {
    val preParams = Some(StackFrameTemplate.STACK_FRAME_ST)
    val params: ISZ[ST] = ISZ(st"${names.cComponentImpl} this")
    val methodName = s"${names.cComponentImpl}_finalise_"
    val finaliseMethod = SeL4NixTemplate.methodSignature(methodName, preParams, params, "Unit")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = fileUri,
      owner = "",
      name = methodName,
      line = 0)

    val ret: ST =
      st"""${finaliseMethod} {
          |  ${declNewStackFrame};
          |
          |  // example finalise method
          |}"""
    return ret
  }

  def getExistingCFiles(cExtensionDir: String): ISZ[Os.Path] = {
    val p = Os.path(cExtensionDir)
    val ret: ISZ[Os.Path] = if (p.exists && p.isDir) {
      p.list.filter(f => f.ext == "c" || f.ext == "h")
    } else {
      ISZ()
    }
    return ret
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
