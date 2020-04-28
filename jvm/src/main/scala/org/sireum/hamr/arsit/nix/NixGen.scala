package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.ir.{Aadl, Component, FeatureCategory}

trait NixGen {
  def dirs: ProjectDirectories
  def cExtensionDir: String
  def model : Aadl
  def arsitOptions: Cli.ArsitOption
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
        
        var headerMethods: ISZ[ST] = ISZ()
        var implMethods: ISZ[ST] = ISZ()
        for (p <- ports) {
          val typeNames = SlangUtil.getDataTypeNames(p._portType, names.basePackage)

          var params = ISZ(st"${componentName} this")

          if (Util.isInFeature(p.feature)) {

            val methodName = Sel4NamesUtil.apiHelperMethodName(s"get_${p.name}", names)
            val returnType = "B"
            val pointer: String = if (typeNames.isEnum() || typeNames.isBaseType()) "*" else ""
            
            if(!typeNames.isEmptyType()) {
              params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"
            }
            
            val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, returnType)
            val apiGetMethodName = s"${names.cBridgeApi}_get${p.name}_"

            headerMethods = headerMethods :+ st"${signature};"

            implMethods = implMethods :+ Sel4NixTemplate.apiGet(
              signature = signature,
              apiGetMethodName = apiGetMethodName,
              c_this = names.cThisApi,
              typ = typeNames)

          } else {
            val isEventPort = p.feature.category == FeatureCategory.EventPort

            val methodName = Sel4NamesUtil.apiHelperMethodName(s"send_${p.name}", names)
            val returnType = "Unit"
            val sendSet: String = if (Util.isDataPort(p.feature)) "set" else "send"
            if (!isEventPort) {
              params = params :+ st"${typeNames.qualifiedCTypeName} value"
            }

            val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, returnType)
            val apiSetMethodName = s"${names.cBridgeApi}_${sendSet}${p.name}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ Sel4NixTemplate.apiSet(signature, apiSetMethodName, names.cThisApi, isEventPort)
          }
        }

        { // logging methods

          val loggers = ISZ("logInfo", "logDebug", "logError")

          for (l <- loggers) {
            val methodName = Sel4NamesUtil.apiHelperMethodName(l, names)
            val params = ISZ(st"${componentName} this", st"String str")

            val signature = Sel4NixTemplate.methodSignature(methodName, None(), params, "Unit")

            val apiLogMethodName = s"${names.cBridgeApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ Sel4NixTemplate.apiLog(signature, apiLogMethodName, names.cThisApi)
          }
        }

        val headerApiFile = extRoot / s"${apiHeaderName}.h"
        val implApiFile = extRoot / s"${apiHeaderName}.c"

        val macroName = SlangUtil.toUpperCase(s"${apiHeaderName}_h")

        val headerContents = Sel4NixTemplate.cHeaderFile(macroName, headerMethods)

        val implContents = Sel4NixTemplate.cImplFile(apiHeaderName, implMethods)

        extensionFiles = extensionFiles :+ headerApiFile
        extensionFiles = extensionFiles :+ implApiFile

        resources = resources :+ SlangUtil.createResource(headerApiFile.up.value, ISZ(headerApiFile.name), headerContents, T)
        resources = resources :+ SlangUtil.createResource(implApiFile.up.value, ISZ(implApiFile.name), implContents, T)
      }
      

      { // add entrypoint stubs

        val preParams: Option[ST] = Some(st"STACK_FRAME")
        val params: ISZ[ST] = ISZ(st"${componentName} this")

        var methods : ISZ[ST] = ISZ(
          genStubInitializeMethod(names, ports), 
          genStubFinaliseMethod(names))

        Util.getDispatchProtocol(c) match {
          case Some(DispatchProtocol.Periodic) =>
            // timetriggered
            val timeTriggered = Sel4NixTemplate.methodSignature(s"${componentName}_timeTriggered_", preParams, params, "Unit")
            methods = methods :+ st"${timeTriggered} {}"

          case Some(DispatchProtocol.Sporadic) =>
            val inEventPorts = ports.filter(f => Util.isEventPort(f.feature) && Util.isInFeature(f.feature))

            for(p <- inEventPorts) {
              val handlerName = s"${componentName}_handle${p.name}"
              var eventDataParams = params
              if(p.feature.category == FeatureCategory.EventDataPort) {
                val typeNames = SlangUtil.getDataTypeNames(p._portType, names.basePackage)
                eventDataParams = eventDataParams :+ st"${typeNames.qualifiedCTypeName} value"
              }
              val handler = Sel4NixTemplate.methodSignature(s"${handlerName}_", preParams, eventDataParams, "Unit")
              val logInfo = Sel4NamesUtil.apiHelperMethodName("logInfo", names);
              methods = methods :+ st"""${handler} {
                                       |  
                                       |  DeclNewString(${p.name}String);
                                       |  String__append((String) &${p.name}String, string("${handlerName} called"));
                                       |  ${logInfo} (this, (String) &${p.name}String);
                                       |}"""
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        val impl = st"""#include <${apiHeaderName}.h>
                       |#include <ext.h>
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
    val initialiseMethod = Sel4NixTemplate.methodSignature(s"${names.cComponentImplQualifiedName}_initialise_", preParams, params, "Unit")

    val loggers: ISZ[String] = ISZ("logInfo", "logDebug", "logError")
    val statements = loggers.map(l => {
      val mname = Sel4NamesUtil.apiHelperMethodName(l, names)
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
    val finaliseMethod = Sel4NixTemplate.methodSignature(s"${names.cComponentImplQualifiedName}_finalise_", preParams, params, "Unit")
    val ret: ST = st"""${finaliseMethod} {
                      |  // example finalise method
                      |}"""
    return ret
  }
}

object NixGenDispatch {

  def generate(dirs: ProjectDirectories,
               model: Aadl,
               arsitOptions: Cli.ArsitOption,
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
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, types, previousPhase).generate()
      case Cli.ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, types, previousPhase).generate()
      case Cli.ArsitPlatform.MacOS =>
        ArtNixGen(dirs, cExtensionDir, model, arsitOptions, types, previousPhase).generate()
      case Cli.ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, cExtensionDir, model, arsitOptions, types, previousPhase).generate()
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
