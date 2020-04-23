package org.sireum.hamr.arsit

import org.sireum._
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

  def getName(s: Name): String = s.name.elements.mkString("_")

  def getLastName(s: Name): String = ISZOps(s.name).last

  @pure def hasProperty(properties: ISZ[Property], propertyName: String): B = {
    return properties.filter(p => getLastName(p.name) == propertyName).nonEmpty
  }
  
  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String): Option[T] = {
    for (p <- properties if getLastName(p.name) == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def getPropertyValues(properties: ISZ[Property], propertyName: String): ISZ[PropertyValue] = {
    return properties.filter(p => getLastName(p.name) == propertyName).flatMap(p => p.propertyValues)
  }

  @pure def getDispatchProtocol(m: Component): Option[DispatchProtocol.Type] = {
    Util.getDiscreetPropertyValue[ValueProp](m.properties, Util.Prop_Thread_Properties__Dispatch_Protocol) match {
      case Some(x) =>
        x.value.toString match {
          case "Sporadic" => return Some(DispatchProtocol.Sporadic)
          case "Periodic" => return Some(DispatchProtocol.Periodic)
          case s =>
            val name: Names = Util.getComponentNames(m, "")
            halt(s"${s} dispatch protocol for ${name.componentImpl} is not currently supported")
        }
      case _ => None[DispatchProtocol.Type]()
    }
  }

  @pure def getSlangEmbeddedDispatchProtocol(m: Component): DispatchProtocol.Type = {
    return Util.getDispatchProtocol(m) match {
      case Some(x) => x
      case _ =>
        if (Util.isDevice(m)) {
          DispatchProtocol.Periodic
        }
        else {
          val name: Names = Util.getComponentNames(m, "")
          halt(s"Dispatch Protocol for ${name.componentImpl} must be Periodic or Sporadic")
        }
    }
  }
  @pure def getPeriod(m: Component): Z = {
    val s: String = Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Prop_Timing_Properties__Period) match {
      case Some(UnitProp(value, Some(org.sireum.String("ps")))) =>
        val v = value.toString.toDouble / 1e9
        s"${v.toLong}"
      case Some(UnitProp(value, Some(org.sireum.String("ms")))) => s"${value.toString.toInt}"
      case _ => "1"
    }
    return Z.apply(s).get
  }

  @pure def getZProperty(key: String, properties: ISZ[Property]): Option[Z] = {
    return Util.getDiscreetPropertyValue[UnitProp](properties, key) match {
      case Some(UnitProp(value, _)) => Some(Z(value.native.toDouble.toInt))
      case None() => None[Z]()
    }
  }

  @pure def getDefaultBitWidth(c: Component): Z = {
    return getZProperty(Util.Prop_HAMR__Default_Bit_Width, c.properties) match {
      case Some(x) => x
      case None() => Util.DEFAULT_BIT_WIDTH;
    }
  }

  @pure def getDefaultMaxSequenceSize(c: Component, altMax: Z): Z = {
    return getZProperty(Util.Prop_HAMR__Default_Max_Sequence_Size, c.properties) match {
      case Some(x) => x
      case None() => altMax
    }
  }

  @pure def getMaxStringSize(c: Component): Z = {
    return getZProperty(Util.Prop_HAMR__Max_String_Size, c.properties) match {
      case Some(x) => x
      case None() => Util.DEFAULT_MAX_STRING_SIZE
    }
  }

  @pure def getStackSizeInBytes(c: Component): Option[Z] = {
    return Util.getDiscreetPropertyValue[UnitProp](c.properties, Util.Prop_Memory_Properties__Stack_Size) match {
      case Some(UnitProp(value, unit)) =>
        R(value) match {
          case Some(v) =>
            val _v: Z = conversions.R.toZ(v)
            unit match {
              case Some(string"bits")  => Some(_v / z"8")
              case Some(string"Bytes") => Some(_v)
              case Some(string"KByte") => Some(_v * z"1000")
              case Some(string"MByte") => Some(_v * z"1000" * z"1000")
              case Some(string"GByte") => Some(_v * z"1000" * z"1000" * z"1000")
              case Some(string"TByte") => Some(_v * z"1000" * z"1000" * z"1000" * z"1000")
              case _ => None[Z]()
            }
          case _ => None[Z]()
        }
      case _ => None[Z]()
    }
  }

  @pure def getDispatchTriggers(c: Component): Option[ISZ[String]] = {
    if(!hasProperty(c.properties, Prop_Thread_Properties__Dispatch_Trigger)){
      return None()
    } else {
      var ret: ISZ[String] = ISZ()
      for (p <- getPropertyValues(c.properties, Prop_Thread_Properties__Dispatch_Trigger)) {
        p match {
          case ReferenceProp(v) => ret = ret :+ Util.getLastName(v)
          case _ => halt(s"Unhandled ${p}")
        }
      }
      return Some(ret)
    }
  }
  
  @pure def getEnumValues(v: Component): ISZ[String] = {
    var ret: ISZ[String] = ISZ()
    if(isEnum(v.properties)) {
      for(p <- getPropertyValues(v.properties, Prop_Data_Model__Enumerators)){
        p match {
          case ValueProp(v) => ret = ret :+ v
          case _ => halt(s"Unhandled ${p}")
        }
      }
    }

    return ret
  }

  @pure def getArrayDimensions(a: ArrayType): ISZ[Z] = {
    return a.container match {
      case Some(c) =>
        getPropertyValues(c.properties, Prop_Data_Model__Dimension).map(_.asInstanceOf[UnitProp]).map(m => {
          R(m.value) match {
            case Some(x) => conversions.R.toZ(x)
            case _ => z"-1"
          }
        })
      case _ => ISZ()
    }
  }

  @pure def findMaxAadlArraySize(types: AadlTypes): Z = {
    var max: Z = 0
    def processType(t: AadlType):Unit = {
      t match {
        case a: ArrayType =>
          val dims = Util.getArrayDimensions(a)
          assert(dims.size == 1)
          if(dims(0) > max) {
            max = dims(0)
          }
          processType(a.baseType)
        case r: RecordType =>
          r.fields.values.foreach(t => processType(t))
        case _ =>
      }
    }

    types.typeMap.values.foreach(t => processType(t))

    return max
  }


  @pure def getArrayBaseType(c: Component): String = {
    for (p <- c.properties if getLastName(p.name) == Prop_Data_Model__Base_Type) {
      return p.propertyValues(0).asInstanceOf[ClassifierProp].name
    }
    halt(s"${c} isn't an array")
  }

  @pure def isEnum(props: ISZ[Property]): B = {
    for (p <- props if getLastName(p.name) == Prop_Data_Model__Data_Representation &&
      ISZOps(p.propertyValues).contains(ValueProp("Enum")))
      return true
    return false
  }

  @pure def isEnumType(c: Component): B = {
    return isEnum(c.properties)
  }

  @pure def isRecordType(c: Component): B = {
    return c.subComponents.nonEmpty
  }

  @pure def isArrayType(c: Component): B = {
    for (p <- c.properties if getLastName(p.name) == Prop_Data_Model__Data_Representation &&
      ISZOps(p.propertyValues).contains(ValueProp("Array")))
      return true
    return false
  }

  @pure def isBaseType(c: Component): B = {
    return StringOps(c.classifier.get.name).startsWith("Base_Types::")
  }

  @pure def getFeatureEndType(f: FeatureEnd, types: AadlTypes): AadlType = {
    return f.classifier match {
      case Some(c) => types.typeMap.get(c.name).get
      case _ => SlangUtil.EmptyType
    }
  }

  // replace '-' and '.' with '_'
  @pure def sanitizeName(s: String): String = s.toString.replaceAll("[\\-|\\.]", "_")

  @pure def getComponentNames(c: Component, basePackage: String): Names = {
    val a: Array[scala.Predef.String] = c.classifier.get.name.toString.split("::")
    assert(a.length == 2)

    val aadlPackage = a(0)
    
    val componentName = sanitizeName(a(1))
    val componentImplName = s"${componentName}_Impl"
    
    val bridgeName = s"${componentName}_Bridge"

    Names(basePackage, aadlPackage, bridgeName, componentName, componentImplName, c)
  }

  @pure def getPackageName(value: String): String =
    value.toString.split("::").dropRight(1).mkString(".")

  def getPorts(m: Component, types: AadlTypes, basePackage: String, counter: Z): ISZ[Port] = {
    var _counter = counter
    
    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m)
    
    var ports: ISZ[Port] = ISZ()
    for (f <- Util.getFeatureEnds(m.features) if Util.isPort(f)) {
      val pType = Util.getFeatureEndType(f, types)
      val portName = Util.getLastName(f.identifier)
      val isTrigger = if (dispatchTriggers.isEmpty) T else {
        dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty
      }
      ports :+= Port(f, m, pType, basePackage, isTrigger, _counter)
      _counter = _counter + 1
    }
    return ports
  }

  @pure def getFeatureEnds(is: ISZ[Feature]): ISZ[FeatureEnd] = is.withFilter(_.isInstanceOf[FeatureEnd]).map(_.asInstanceOf[FeatureEnd])

  @pure def isPort(f: FeatureEnd): B = isEventPort(f) || isDataPort(f)

  @pure def isEventPort(f: FeatureEnd): B =
    f.category == FeatureCategory.EventDataPort || f.category == FeatureCategory.EventPort

  @pure def isEventDataPort(f: FeatureEnd): B = f.category == FeatureCategory.EventDataPort

  @pure def isDataPort(f: FeatureEnd): B = f.category == FeatureCategory.DataPort

  @pure def isInFeature(f: FeatureEnd): B = f.direction == Direction.In

  @pure def isOutFeature(f: FeatureEnd): B = f.direction == Direction.Out

  @pure def isThread(c: Component): B = c.category == ComponentCategory.Thread

  @pure def isDevice(c: Component): B = c.category == ComponentCategory.Device

  @pure def isSystem(c: Component): B = c.category == ComponentCategory.System

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

object TypeResolver {

  def getSlangType(s: String): SlangType.Type = {
    val t: SlangType.Type = s match {
      case org.sireum.String("Boolean") => SlangType.B

      case org.sireum.String("Integer") => SlangType.Z
        
      case org.sireum.String("Integer_8") => SlangType.S8
      case org.sireum.String("Integer_16") => SlangType.S16
      case org.sireum.String("Integer_32") => SlangType.S32
      case org.sireum.String("Integer_64") => SlangType.S64

      case org.sireum.String("Unsigned_8") => SlangType.U8
      case org.sireum.String("Unsigned_16") => SlangType.U16
      case org.sireum.String("Unsigned_32") => SlangType.U32
      case org.sireum.String("Unsigned_64") => SlangType.U64

      case org.sireum.String("Float") => SlangType.R // TODO
      case org.sireum.String("Float_32") => SlangType.F32
      case org.sireum.String("Float_64") => SlangType.F64

      case org.sireum.String("Character") => SlangType.C
      case org.sireum.String("String") => SlangType.String
    }
    return t
  }

  def processDataTypes(values: ISZ[Component], basePackage: String): AadlTypes = {
    var typeMap: Map[String, AadlType] = Map.empty
    
    for (v <- values) {
      typeMap = typeMap + (v.classifier.get.name ~> processType(v, basePackage, typeMap))
    }
    return AadlTypes(typeMap)
  }

  def processType(c: Component, basePackage: String, typeMap: Map[String, AadlType]): AadlType = {
    assert(c.category == ComponentCategory.Data)
    val cname = c.classifier.get.name

    val container = Some(c)

    if(Util.isEnumType(c)) {

      return  EnumType(cname, container, Util.getEnumValues(c))

    } else if(Util.isBaseType(c)) {

      val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")

      val t: SlangType.Type = TypeResolver.getSlangType(aadlType)

      return BaseType(cname, container, t)

    } else if(Util.isArrayType(c)) {

      val baseTypeName = Util.getArrayBaseType(c)
      val baseType = typeMap.get(baseTypeName).get

      return ArrayType(cname, container, baseType)
    } else if(Util.isRecordType(c)) {
      var fields: Map[String, AadlType] = Map.empty

      for(sc <- c.subComponents){
        val fieldName = Util.getLastName(sc.identifier)
        fields = fields + (fieldName ~> processType(sc, basePackage, typeMap))
      }

      return RecordType(cname, container, fields)

    } else {
      return TODOType(cname, container)
    }
  }
}

