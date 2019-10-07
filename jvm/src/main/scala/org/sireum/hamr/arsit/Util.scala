package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir._
import org.sireum.message.Reporter
import org.sireum.ops._

object Util {
  var reporter: Reporter = Reporter.create
  var toolName: String = "Arsit"

  var verbose: B = F

  val Prop_Thread_Properties__Dispatch_Protocol: String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Thread_Properties__Urgency: String = "Thread_Properties::Urgency"

  val Prop_Timing_Properties__Period: String = "Timing_Properties::Period"

  val Prop_Data_Model__Base_Type: String = "Data_Model::Base_Type"
  val Prop_Data_Model__Data_Representation: String = "Data_Model::Data_Representation"
  val Prop_Data_Model__Dimension: String = "Data_Model::Dimension"
  val Prop_Data_Model__Element_Names: String = "Data_Model::Element_Names"
  val Prop_Data_Model__Enumerators: String = "Data_Model::Enumerators"

  val Prop_HAMR__OS: String = "HAMR::OS";
  val Prop_HAMR__HW: String = "HAMR::HW";
  val Prop_HAMR__Default_Max_Sequence_Size: String = "HAMR::Default_Max_Sequence_Size";
  val Prop_HAMR__Default_Bit_Width: String = "HAMR::Default_Bit_Width";
  val Prop_HAMR__Max_String_Size: String = "HAMR::Max_String_Size";

  val DEFAULT_BIT_WIDTH: Z = 64
  val DEFAULT_MAX_STRING_SIZE: Z = 256

  val EmptyType = TODOType("--EmptyType--", None())
  val EmptyTypeNames = DataTypeNames(EmptyType, "", "art", "Empty")

  def isEmptyType(name : String): B = {
    return name == EmptyTypeNames.qualifiedTypeName
  }

  def isEmptyType(t : DataTypeNames): B = {
    return t.typ == EmptyType
  }

  def getName(s: Name): String = s.name.elements.mkString("_")

  def getLastName(s: Name): String = ISZOps(s.name).last

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
            val name: Names = Util.getNamesFromClassifier(m.classifier.get, "")
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
          val name: Names = Util.getNamesFromClassifier(m.classifier.get, "")
          halt(s"Dispatch Protocol for ${name.componentImpl} must be Periodic or Sporadic")
        }
    }
  }
  @pure def getPeriod(m: Component): String = {
    return Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Prop_Timing_Properties__Period) match {
      case Some(UnitProp(value, Some(org.sireum.String("ps")))) =>
        val v = value.toString.toDouble / 1e9
        s"${v.toLong}"
        /*
        assert(x.unit.get == org.sireum.String("ps"))
        // convert picoseconds to milliseconds.  x.value was a double in osate
        // ps, ns => ps * 1000, us => ns * 1000, ms => us * 1000
        val v = x.value.toString.toDouble / 1e9
        s"${v.toLong}"
        */
      case Some(UnitProp(value, Some(org.sireum.String("ms")))) => s"${value.toString.toInt}"
      case _ => "1"
    }
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


  @pure def getDataTypeNames(typ: AadlType, topPackage: String): DataTypeNames = {
    val (packageName, typeName) = if(typ == Util.EmptyType) {
      ("art", "Empty")
    } else {
      val classifier = typ.container.get.classifier.get

      val a = classifier.name.toString.split("::")
      assert(a.size == 2)

      (a(0), a(1))
    }
    return DataTypeNames(typ, topPackage, packageName, sanitizeName(typeName))
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
      case _ => Util.EmptyType
    }
  }

  // replace '-' and '.' with '_'
  @pure def sanitizeName(s: String): String = s.toString.replaceAll("[\\-|\\.]", "_")

  @pure def getNamesFromClassifier(c: Classifier, basePackage: String): Names = {
    val a: Array[scala.Predef.String] = c.name.toString.split("::")
    assert(a.length == 2)
    val compName = sanitizeName(a(1))
    Names(s"${basePackage}.${a(0)}", s"${basePackage}/${a(0)}", compName + "_Bridge", compName, compName + "_Impl")
  }

  @pure def getPackageName(value: String): String =
    value.toString.split("::").dropRight(1).mkString(".")


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

  @pure def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: String): ISZ[Resource] = {
    var resources: ISZ[Resource] = ISZ()
    for((p, c) <- Library.getFiles if p.native.contains("art")) {
      val _c = if(p.native.contains("Art.scala")) {
        val out = new StringBuilder()
        c.native.split("\n").map(s =>
          out.append(
            if(s.contains("val maxComponents")) {
              s"  val maxComponents: BridgeId = $maxComponent"
            } else if (s.contains("val maxPorts:")) {
              s"  val maxPorts: PortId = $maxPort"
            } else {
              s
            }).append("\n"))
        out.toString()
      } else {
        c
      }
      resources = resources :+ SlangUtil.createResource(outputDir, ISZ(p), st"${_c}", T)
    }
    return resources
  }

  @pure def getLibraryFile(fileName: String): ST = {
    val e = Library.getFiles.filter(p => p._1 == fileName)
    assert(e.length == 1)
    return st"${e(0)._2}"
  }

  @pure def getIpc(ipcmech: Cli.IpcMechanism.Type , packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r = ipcmech match {
      case Cli.IpcMechanism.SharedMemory => "ipc_shared_memory.c"
      case Cli.IpcMechanism.MessageQueue => "ipc_message_queue.c"
    }
    val c = getLibraryFile(r).render.native.replaceAll(PACKAGE_PLACEHOLDER, packageName.native)
    st"${c}"
  }
}

object TypeResolver {

  var typeMap: Map[String, AadlType] = Map.empty

  def getSlangType(s: String): SlangType.Type = {
    val t: SlangType.Type = s match {
      case org.sireum.String("Boolean") => SlangType.B

      case org.sireum.String("Integer") => SlangType.Z
      case org.sireum.String("Integer_8") => SlangType.Z8
      case org.sireum.String("Integer_16") => SlangType.Z16
      case org.sireum.String("Integer_32") => SlangType.Z32
      case org.sireum.String("Integer_64") => SlangType.Z64

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
    for (v <- values) {
      typeMap = typeMap + (v.classifier.get.name ~> processType(v, basePackage))
    }
    return AadlTypes(typeMap)
  }

  def processType(c: Component, basePackage: String): AadlType = {
    assert(c.category == ComponentCategory.Data)
    val cname = c.classifier.get.name
    val names = Util.getNamesFromClassifier(c.classifier.get, basePackage)

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
        fields = fields + (fieldName ~> processType(sc, basePackage))
      }

      return RecordType(cname, container, fields)

    } else {
      return TODOType(cname, container)
    }
  }
}

case class Names(packageName : String,
                 packagePath : String,
                 bridge: String,
                 component: String,
                 componentImpl: String)

case class DataTypeNames(typ: AadlType,
                         basePackage: String,
                         packageName: String,
                         typeName: String) {

  def filePath: String = s"$basePackage/$packageName/$typeName.scala"

  def qualifiedPackageName: String = s"$basePackage.$packageName"

  def qualifiedTypeName: String = s"$packageName.$typeName"

  def referencedTypeName: String = typeName + (if(isEnum) ".Type" else "")
  def qualifiedReferencedTypeName: String = s"${packageName}.${referencedTypeName}"

  def payloadName: String = if(packageName == String("art") && typeName == String("Empty")) typeName else s"${typeName}_Payload"
  def qualifiedPayloadName: String = s"${packageName}.${payloadName}"

  def isEnum():B = {
    return typ.isInstanceOf[EnumType]
  }

  def empty(): String = {
    val ret: String = typ match {
      case e:EnumType => s"${qualifiedTypeName}.byOrdinal(0).get"
      case e:BaseType => s"${qualifiedTypeName}_empty()"
      case e:ArrayType => s"${qualifiedTypeName}.empty()"
      case e:RecordType => s"${qualifiedTypeName}.empty()"
      case e:TODOType => s"${qualifiedTypeName}.empty()"
    }
    return ret
  }
}

case class Port(feature: FeatureEnd, parent: Component, _portType: AadlType, basePackageName: String){

  def name: String = Util.getLastName(feature.identifier)
  def path: String = Util.getName(feature.identifier)

  def parentName: String = Util.getLastName(parent.identifier)
  def parentPath: String = Util.getName(parent.identifier)

  def portType: DataTypeNames = Util.getDataTypeNames(_portType, basePackageName)

  def urgency: Z = Util.getDiscreetPropertyValue[UnitProp](feature.properties, Util.Prop_Thread_Properties__Urgency) match {
    case Some(v) => v.value.toString.toDouble.toInt
    case _ => 0
  }
}
