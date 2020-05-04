package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.codegen.common.{AadlType, AadlTypes, ArrayType, BaseType, CommonUtil, EnumType, Names, PropertyUtil, RecordType, SlangType, TODOType, TypeUtil}
import org.sireum.hamr.ir._
import org.sireum.message.Reporter
import org.sireum.ops._

object Util {
  
  var reporter: Reporter = Reporter.create
  var toolName: String = "Arsit"

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  var verbose: B = F

  val Prop_Thread_Properties__Dispatch_Protocol: String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Thread_Properties__Urgency: String = "Thread_Properties::Urgency"
  val Prop_Thread_Properties__Dispatch_Trigger: String = "Thread_Properties::Dispatch_Trigger"
  
  val Prop_Timing_Properties__Period: String = "Timing_Properties::Period"

  val Prop_Data_Model__Base_Type: String = "Data_Model::Base_Type"
  val Prop_Data_Model__Data_Representation: String = "Data_Model::Data_Representation"
  val Prop_Data_Model__Dimension: String = "Data_Model::Dimension"
  val Prop_Data_Model__Element_Names: String = "Data_Model::Element_Names"
  val Prop_Data_Model__Enumerators: String = "Data_Model::Enumerators"

  val Prop_Memory_Properties__Stack_Size = "Memory_Properties::Stack_Size"
  
  val Prop_HAMR__OS: String = "HAMR::OS";
  val Prop_HAMR__HW: String = "HAMR::HW";
  val Prop_HAMR__Default_Max_Sequence_Size: String = "HAMR::Default_Max_Sequence_Size";
  val Prop_HAMR__Default_Bit_Width: String = "HAMR::Default_Bit_Width";
  val Prop_HAMR__Max_String_Size: String = "HAMR::Max_String_Size";

  val DEFAULT_BIT_WIDTH: Z = 64
  val DEFAULT_MAX_STRING_SIZE: Z = 256

  @pure def hasProperty(properties: ISZ[Property], propertyName: String): B = {
    return properties.filter(p => CommonUtil.getLastName(p.name) == propertyName).nonEmpty
  }
  
  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String): Option[T] = {
    for (p <- properties if CommonUtil.getLastName(p.name) == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def getPropertyValues(properties: ISZ[Property], propertyName: String): ISZ[PropertyValue] = {
    return properties.filter(p => CommonUtil.getLastName(p.name) == propertyName).flatMap(p => p.propertyValues)
  }

  @pure def getPeriod(m: Component): Z = {
    PropertyUtil.getPeriod(m) match {
      case Some(z) => z
      case _ => z"1"
    }
  }

  @pure def getDefaultBitWidth(c: Component): Z = {
    return PropertyUtil.getUnitPropZ(c.properties, Util.Prop_HAMR__Default_Bit_Width) match {
      case Some(x) => x
      case None() => Util.DEFAULT_BIT_WIDTH;
    }
  }

  @pure def getDefaultMaxSequenceSize(c: Component, altMax: Z): Z = {
    return PropertyUtil.getUnitPropZ(c.properties, Util.Prop_HAMR__Default_Max_Sequence_Size) match {
      case Some(x) => x
      case None() => altMax
    }
  }

  @pure def getMaxStringSize(c: Component): Z = {
    return PropertyUtil.getUnitPropZ(c.properties, Util.Prop_HAMR__Max_String_Size) match {
      case Some(x) => x
      case None() => Util.DEFAULT_MAX_STRING_SIZE
    }
  }

  @pure def getDispatchTriggers(c: Component): Option[ISZ[String]] = {
    if(!hasProperty(c.properties, Prop_Thread_Properties__Dispatch_Trigger)){
      return None()
    } else {
      var ret: ISZ[String] = ISZ()
      for (p <- getPropertyValues(c.properties, Prop_Thread_Properties__Dispatch_Trigger)) {
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

    val pType: AadlType = if(types.rawConnections) {
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
