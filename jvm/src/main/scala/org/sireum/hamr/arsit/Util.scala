package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.message.Reporter
import org.sireum.ops._

object Util {
  
  var reporter: Reporter = Reporter.create
  var toolName: String = "Arsit"

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  var verbose: B = F

  @pure def getPeriod(m: Component): Z = {
    PropertyUtil.getPeriod(m) match {
      case Some(z) => z
      case _ => z"1"
    }
  }

  @pure def getDispatchTriggers(c: Component): Option[ISZ[String]] = {
    if(!PropertyUtil.hasProperty(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)){
      return None()
    } else {
      var ret: ISZ[String] = ISZ()
      for (p <- PropertyUtil.getPropertyValues(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)) {
        p match {
          case ReferenceProp(v) => ret = ret :+ CommonUtil.getLastName(v)
          case _ => halt(s"Unhandled ${p}")
        }
      }
      return Some(ret)
    }
  }

  @pure def getFeatureEndType(f: FeatureEnd, types: AadlTypes): AadlType = {
    return f.classifier match {
      case Some(c) => types.typeMap.get(c.name).get
      case _ => TypeUtil.EmptyType
    }
  }

  def getPort(feature: FeatureEnd,
              parent: Component,
              types: AadlTypes,
              basePackage: String,
              isTrigger: B,
              counter: Z): Port = {

    val pType: AadlType = if(types.rawConnections && CommonUtil.isDataPort(feature)) {
      TypeUtil.SlangEmbeddedBitType
    } else {
      Util.getFeatureEndType(feature, types)
    }

    return Port(feature, parent, pType, basePackage, isTrigger, counter)
  }

  def getPorts(m: Component, types: AadlTypes, basePackage: String, counter: Z): ISZ[Port] = {
    var _counter = counter
    
    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m)
    
    var ports: ISZ[Port] = ISZ()
    for (f <- Util.getFeatureEnds(m.features) if CommonUtil.isPort(f)) {
      val portName = CommonUtil.getLastName(f.identifier)
      val isTrigger = if (dispatchTriggers.isEmpty) T else {
        dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
      }
      ports :+= getPort(f, m, types, basePackage, isTrigger, _counter)

      _counter = _counter + 1
    }
    return ports
  }

  @pure def getFeatureEnds(is: ISZ[Feature]): ISZ[FeatureEnd] = is.withFilter(_.isInstanceOf[FeatureEnd]).map(_.asInstanceOf[FeatureEnd])

  @pure def doNotEditComment(from: Option[String] = None[String]()) = {
    val _from = if (from.nonEmpty) " from " + from.get else ""
    s"// This file was auto-generated${_from}.  Do not edit"
  }

  @pure def safeToEditComment(): String = {
    "// the contents of this file will not be overwritten"
  }
  
  @pure def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for((p, c) <- ArsitLibrary.getFiles if p.native.contains("art")) {
      val _c = 
        if(p.native.contains("Art.scala")) {
          val out = new StringBuilder()
          c.native.split("\n").map(s =>
            out.append({
              if (s.contains("val maxComponents")) {
                s"  val maxComponents: BridgeId = $maxComponent"
              } else if (s.contains("val maxPorts:")) {
                s"  val maxPorts: PortId = $maxPort"
              } /* 
              else if (s.contains("ArtNative.logInfo(logTitle")) {
                val pos = s.indexOf("ArtNative.logInfo")
                val first = s.substring(0, pos)
                val last = s.substring(pos)
                s"${first}//${last}"                
              } */
              else {
                s
              }
            }).append("\n"))
          out.toString()
        } else {
          c
        }
      
      resources = resources :+ SlangUtil.createResource(outputDir, ISZ(p), st"${_c}", T)
    }
    return resources
  }

  @pure def getIpc(ipcmech: Cli.IpcMechanism.Type , packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r = ipcmech match {
      case Cli.IpcMechanism.SharedMemory => "ipc_shared_memory.c"
      case Cli.IpcMechanism.MessageQueue => "ipc_message_queue.c"
    }
    val c = SlangUtil.getLibraryFile(r).render.native.replaceAll(PACKAGE_PLACEHOLDER, packageName.native)
    st"${c}"
  }
}
