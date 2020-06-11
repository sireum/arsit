package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.{SeL4NixNamesUtil, SeL4NixTemplate, StringTemplate}
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.{CommonUtil, Names, StringUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlTypes, TypeUtil}
import org.sireum.hamr.ir.{Aadl, Component, FeatureCategory}

trait NixGen {
  def dirs: ProjectDirectories
  def cExtensionDir: String
  def model : Aadl
  def arsitOptions: Cli.ArsitOption
  def symbolTable: SymbolTable
  def types: AadlTypes
  def previousPhase: Result
  
  def generate(): ArsitResult

  def genExtensionFiles(c: Component, names: Names, ports: ISZ[Port]): (ISZ[Os.Path], ISZ[Resource]) = {
    
    val rootExtDir = Os.path(cExtensionDir)
    
    var extensionFiles: ISZ[Os.Path] = ISZ()
    var resources: ISZ[Resource] = ISZ()

    val extC = rootExtDir / "ext.c"
    val extH = rootExtDir / "ext.h"
    resources = resources :+ SlangUtil.createResource(extC.up.value, ISZ(extC.name), SlangUtil.getLibraryFile("ext.c"), F)
    resources = resources :+ SlangUtil.createResource(extH.up.value, ISZ(extH.name), SlangUtil.getLibraryFile("ext.h"), F)

    extensionFiles = (extensionFiles :+ extC) :+ extH

    if(arsitOptions.excludeImpl) {

      val componentName = names.cComponentImplQualifiedName
      val extRoot = rootExtDir / names.componentImpl
      val apiHeaderName = s"${names.componentImpl}_api"
      
      { // api helper methods
        
        var headerMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")
        var implMethods: ISZ[ST] = ISZ(st"${StringTemplate.doNotEditComment(None())}")

        for (p <- ports) {
          val typeNames = SlangUtil.getDataTypeNames(p._portType, names.basePackage)

          if (CommonUtil.isInFeature(p.feature)) {

            val cApiMethodName = SeL4NixNamesUtil.apiHelperMethodName(s"get_${p.name}", names)
            val returnType = "bool"
            val slangApiGetMethodName = s"${names.cBridgeApi}_get${p.name}_"

            if(types.rawConnections && CommonUtil.isDataPort(p.feature)) {
              // provide alt byte array version

              val altParams = ISZ(
                st"${componentName} this",
                st"size_t *numBits",
                st"uint8_t *byteArray")

              val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, None(), altParams, returnType)

              headerMethods = headerMethods :+ st"${altSignature};"

              implMethods = implMethods :+ SeL4NixTemplate.apiGet_byteArrayVersion(
                signature = altSignature,
                apiGetMethodName = slangApiGetMethodName,
                c_this = names.cThisApi,
                typ = typeNames)
            } else {
              val pointer: String = if (typeNames.isEnum() || typeNames.isBaseType()) "*" else ""

              var params = ISZ(st"${componentName} this")
              if(!typeNames.isEmptyType()) {
                params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"
              }

              val signature = SeL4NixTemplate.methodSignature(cApiMethodName, None(), params, returnType)

              headerMethods = headerMethods :+ st"${signature};"

              implMethods = implMethods :+ SeL4NixTemplate.apiGet(
                signature = signature,
                apiGetMethodName = slangApiGetMethodName,
                c_this = names.cThisApi,
                typ = typeNames)
            }
          } else {
            val isEventPort = p.feature.category == FeatureCategory.EventPort

            val cApiMethodName = SeL4NixNamesUtil.apiHelperMethodName(s"send_${p.name}", names)
            val returnType = "void"
            val sendSet: String = if (CommonUtil.isAadlDataPort(p.feature)) "set" else "send"
            val slangApiSetMethodName = s"${names.cBridgeApi}_${sendSet}${p.name}_"

            if(types.rawConnections && CommonUtil.isDataPort(p.feature)) {
              // provide alt byte array version
              val altParams = ISZ(
                st"${componentName} this",
                st"size_t numBits",
                st"uint8_t *byteArray"
              )
              val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, None(), altParams, returnType)
              headerMethods = headerMethods :+ st"${altSignature};"
              implMethods = implMethods :+ SeL4NixTemplate.apiSet_byteArrayVersion(
                altSignature,
                slangApiSetMethodName,
                names.cThisApi)
            } else {

              var params = ISZ(st"${componentName} this")
              if (!isEventPort) {
                params = params :+ st"${typeNames.qualifiedCTypeName} value"
              }

              val signature = SeL4NixTemplate.methodSignature(cApiMethodName, None(), params, returnType)

              headerMethods = headerMethods :+ st"${signature};"
              implMethods = implMethods :+ SeL4NixTemplate.apiSet(
                signature,
                slangApiSetMethodName,
                names.cThisApi,
                isEventPort)
            }
          }
        }

        { // logging methods

          val loggers = ISZ("logInfo", "logDebug", "logError")

          for (l <- loggers) {
            val methodName = SeL4NixNamesUtil.apiHelperMethodName(l, names)
            val params = ISZ(st"${componentName} this", st"String str")

            val signature = SeL4NixTemplate.methodSignature(methodName, None(), params, "void")

            val apiLogMethodName = s"${names.cBridgeApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ SeL4NixTemplate.apiLog(signature, apiLogMethodName, names.cThisApi)
          }
        }

        val headerApiFile = extRoot / s"${apiHeaderName}.h"
        val implApiFile = extRoot / s"${apiHeaderName}.c"

        val macroName = StringUtil.toUpperCase(s"${apiHeaderName}_h")

        val headerContents = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

        val implContents = SeL4NixTemplate.cImplFile(apiHeaderName, implMethods)

        extensionFiles = extensionFiles :+ headerApiFile
        extensionFiles = extensionFiles :+ implApiFile

        resources = resources :+ SlangUtil.createResource(headerApiFile.up.value, ISZ(headerApiFile.name), headerContents, T)
        resources = resources :+ SlangUtil.createResource(implApiFile.up.value, ISZ(implApiFile.name), implContents, T)
      } // end helper api methods
      

      { // add entrypoint stubs

        val preParams: Option[ST] = Some(st"STACK_FRAME")
        val params: ISZ[ST] = ISZ(st"${componentName} this")

        var methods : ISZ[ST] = ISZ(
          genStubInitializeMethod(names, ports), 
          genStubFinaliseMethod(names))

        PropertyUtil.getDispatchProtocol(c) match {
          case Some(Dispatch_Protocol.Periodic) =>
            // timetriggered
            val timeTriggered = SeL4NixTemplate.methodSignature(s"${componentName}_timeTriggered_", preParams, params, "Unit")
            methods = methods :+ st"${timeTriggered} {}"

          case Some(Dispatch_Protocol.Sporadic) =>
            val inEventPorts = ports.filter(f => CommonUtil.isEventPort(f.feature) && CommonUtil.isInFeature(f.feature))

            for(p <- inEventPorts) {
              val handlerName = s"${componentName}_handle${p.name}"
              var eventDataParams = params
              if(p.feature.category == FeatureCategory.EventDataPort) {
                val typeNames = SlangUtil.getDataTypeNames(p._portType, names.basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handler = SeL4NixTemplate.methodSignature(s"${handlerName}_", preParams, eventDataParams, "Unit")
              val logInfo = SeL4NixNamesUtil.apiHelperMethodName("logInfo", names);

              if(types.rawConnections && p.feature.category == FeatureCategory.EventDataPort) {
                val rawHandlerMethodName = s"${handlerName}_raw"

                val rawParams: ISZ[ST] = params :+ st"size_t numBits" :+ st"uint8_t *byteArray"

                val rawHandler = SeL4NixTemplate.methodSignature(rawHandlerMethodName, preParams, rawParams, "Unit")

                methods = methods :+ st"""${rawHandler} {
                                         |
                                         |  DeclNewString(${p.name}String);
                                         |  String__append((String) &${p.name}String, string("${rawHandlerMethodName} called"));
                                         |  ${logInfo} (this, (String) &${p.name}String);
                                         |}"""

                methods = methods :+ st"""${handler} {
                                         |  ${rawHandlerMethodName}(SF this, value->size, value->value);
                                         |}"""
              }
              else {
                methods = methods :+ st"""${handler} {
                                         |
                                         |  DeclNewString(${p.name}String);
                                         |  String__append((String) &${p.name}String, string("${handlerName} called"));
                                         |  ${logInfo} (this, (String) &${p.name}String);
                                         |}"""
              }
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        val impl = st"""#include <${apiHeaderName}.h>
                       |#include <ext.h>
                       |
                       |${StringTemplate.safeToEditComment()}
                       |
                       |${(methods, "\n\n")}
                       |"""

        val implFile = extRoot / s"${names.componentImpl}.c"
        extensionFiles = extensionFiles :+ implFile
        resources = resources :+ SlangUtil.createResource(implFile.up.value, ISZ(implFile.name), impl, F)
      }
    }
    
    return (extensionFiles, resources)
  }

  def genStubInitializeMethod(names: Names, ports: ISZ[Port]): ST = {
    val preParams = Some(st"STACK_FRAME")
    val params: ISZ[ST] = ISZ(st"${names.cComponentImplQualifiedName} this")
    val initialiseMethod = SeL4NixTemplate.methodSignature(s"${names.cComponentImplQualifiedName}_initialise_", preParams, params, "Unit")

    val loggers: ISZ[String] = ISZ("logInfo", "logDebug", "logError")
    val statements = loggers.map(l => {
      val mname = SeL4NixNamesUtil.apiHelperMethodName(l, names)
      st"""${mname}(this, string("Example ${l}"));"""
    })
    val ret: ST = st"""${initialiseMethod} {
                      | // example api usage
                      |  
                      | ${(statements, "\n")}
                      |}"""
    return ret
  }

  def genStubFinaliseMethod(names: Names): ST = {
    val preParams = Some(st"STACK_FRAME")
    val params: ISZ[ST] = ISZ(st"${names.cComponentImplQualifiedName} this")
    val finaliseMethod = SeL4NixTemplate.methodSignature(s"${names.cComponentImplQualifiedName}_finalise_", preParams, params, "Unit")
    val ret: ST = st"""${finaliseMethod} {
                      |  // example finalise method
                      |}"""
    return ret
  }

  def getExistingCFiles(cExtensionDir: String): ISZ[Os.Path] = {
    val p = Os.path(cExtensionDir)
    val ret: ISZ[Os.Path] = if(p.exists && p.isDir) {
      p.list.filter(f => f.ext.native == "c" || f.ext.native == "h")
    } else {
      ISZ()
    }
    return ret
  }
}

object NixGenDispatch {

  def generate(dirs: ProjectDirectories,
               model: Aadl,
               arsitOptions: Cli.ArsitOption,
               symbolTable: SymbolTable,
               types: AadlTypes,
               previousPhase: Result): ArsitResult = {

    val cExtensionDir: String = if (arsitOptions.auxCodeDir.nonEmpty) {
      assert(arsitOptions.auxCodeDir.size == 1)
      arsitOptions.auxCodeDir(0)
    } else {
      SlangUtil.pathAppend(dirs.srcDir, ISZ("c", "ext-c"))
    }
    
    return arsitOptions.platform match {
      case Cli.ArsitPlatform.Linux =>
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, symbolTable, types, previousPhase).generate()
      case Cli.ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, symbolTable, types, previousPhase).generate()
      case Cli.ArsitPlatform.MacOS =>
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, symbolTable, types, previousPhase).generate()
      case Cli.ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, cExtensionDir, model, arsitOptions, symbolTable, types, previousPhase).generate()
      case _ =>
        ArsitResult(
          previousPhase.resources,
          previousPhase.maxPort,
          previousPhase.maxComponent,
          ISZ()
        )
    }
  }
}
