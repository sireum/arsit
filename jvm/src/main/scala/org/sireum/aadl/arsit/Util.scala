package org.sireum.aadl.arsit

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

object Util {
  var verbose: B = F

  val Prop_DispatchProtocol: String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Period: String = "Timing_Properties::Period"
  val Prop_DataRepresentation: String = "Data_Model::Data_Representation"
  val Prop_Urgency: String = "Thread_Properties::Urgency"
  val Prop_Data_Model__Element_Names: String = "Data_Model::Element_Names"
  val Prop_Data_Model__Enumerators: String = "Data_Model::Enumerators"
  val Prop_DataModel__Base_Type: String = "Data_Model::Base_Type"

  val EmptyTypeNames = DataTypeNames("", "", "art", "Empty", false)
  def isEmptyType(name : String) = name == EmptyTypeNames.qualifiedTypeName
  def isEmptyType(t : DataTypeNames) = t == EmptyTypeNames

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

  @pure def getPeriod(m: Component): String = {
    return Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Prop_Period) match {
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

  @pure def getArrayBaseType(c: Component): String = {
    for (p <- c.properties if getLastName(p.name) == Prop_DataModel__Base_Type) {
      return p.propertyValues(0).asInstanceOf[ClassifierProp].name
    }
    halt(s"${c} isn't an array")
  }

  @pure def isEnum(props: ISZ[Property]): B = {
    for (p <- props if getLastName(p.name) == Prop_DataRepresentation &&
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
    for (p <- c.properties if getLastName(p.name) == Prop_DataRepresentation &&
      ISZOps(p.propertyValues).contains(ValueProp("Array")))
      return true
    return false
  }

  @pure def isBaseType(c: Component): B = {
    return StringOps(c.classifier.get.name).startsWith("Base_Types::")
  }

  // replace '-' and '.' with '_'
  @pure def sanitizeName(s: String): String = s.toString.replaceAll("[\\-|\\.]", "_")

  @pure def getNamesFromClassifier(c: Classifier, basePackage: String): Names = {
    val a: Array[scala.Predef.String] = c.name.toString.split("::")
    assert(a.length == 2)
    val compName = sanitizeName(a(1)) //a(1).replaceAll("\\.", "_")
    Names(s"${basePackage}.${a(0)}", s"${basePackage}/${a(0)}", compName + "_Bridge", compName, compName + "_Impl")
  }

  @pure def getDataTypeNames(typ: AadlType, topPackage: String): DataTypeNames = {
    typ match {
      case e: BaseType =>
        return DataTypeNames(qualifiedName = typ.container.classifier.get.name,
          basePackage = topPackage,
          packageName = "Base_Types",
          typeName = e.typeName,
          F)
      case _ =>
        val classifier = typ.container.classifier.get

        val a = classifier.name.toString.split("::")
        assert(a.size == 2)

        val dt = DataTypeNames(classifier.name, topPackage, a(0), sanitizeName(a(1)), typ.isInstanceOf[EnumType])

        return dt
    }
  }

  @pure def getDataTypeNames(f: FeatureEnd, topPackage: String): DataTypeNames = {
    return f.classifier match {
      case Some(c) =>
        val a = c.name.toString.split("::")
        assert(a.size == 2)
        DataTypeNames(c.name, topPackage, a(0), sanitizeName(a(1)), isEnum(f.properties))
      case _ =>
        EmptyTypeNames
    }
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

  @pure def doNotEditComment(from: Option[String] = None[String]()) = {
    val _from = if (from.nonEmpty) " from " + from.get else ""
    s"// This file was auto-generated${_from}.  Do not edit"
  }

  @pure def writeFile(fname: File, st: ST, overwrite: Boolean = true): Unit = {
    try {
      // try building any missing subdirs
      fname.getParentFile.mkdirs

      assert(fname.getParentFile.exists)

      if (overwrite || !fname.exists) {
        val bw = new BufferedWriter(new FileWriter(fname))
        bw.write(st.render.toString)
        bw.close()

        report("Wrote: " + fname, T)
      }
    } catch {
      case e: Throwable =>
        reportError("Error encountered while trying to create file: " + fname)
        reportError(e.getMessage)
    }
  }

  @pure def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: File): Unit = {
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
      Util.writeFile(new File(outputDir, s"${p}"), st"""${_c}""", true)
    }
  }

  @pure def getLibraryFile(fileName: String): ST = {
    val e = Library.getFiles.filter(p => p._1 == fileName)
    assert(e.length == 1)
    return st"${e(0)._2}"
  }

  @pure def getIpc(ipcmech: Cli.Ipcmech.Type , packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r = ipcmech match {
      case Cli.Ipcmech.SharedMemory => "ipc_shared_memory.c"
      case Cli.Ipcmech.MessageQueue => "ipc_message_queue.c"
    }
    val c = getLibraryFile(r).render.native.replaceAll(PACKAGE_PLACEHOLDER, packageName.native)
    st"${c}"
  }

  def report(msg: String, canSupress: B): Unit = {
    if(!canSupress || verbose) {
      Console.println(msg);
    }
  }

  def reportError(str: String): Unit = {
    Console.err.println(str)
  }
}

case class Names(packageName : String,
                 packagePath : String,
                 bridge: String,
                 component: String,
                 componentImpl: String)

case class DataTypeNames(qualifiedName: String,
                         basePackage: String,
                         packageName: String,
                         typeName: String,
                         isEnum: B) {

  def sanitizedName: String = typeName //if(typeName.native == "Map") "Map_" else typeName

  def filePath: String = s"$basePackage/$packageName/$sanitizedName.scala"

  def qualifiedPackageName: String = s"$basePackage.$packageName"

  def qualifiedTypeName: String = s"$packageName.$sanitizedName"

  def referencedTypeName: String = sanitizedName + (if(isEnum) ".Type" else "")
  def qualifiedReferencedTypeName: String = s"${packageName}.${referencedTypeName}"

  def baseQualifiedReferencedTypeName: String = {
    if(packageName.native == "Base_Types") {
      s"org.sireum.${TypeResolver.getSlangType(referencedTypeName)}"
    } else {
      qualifiedReferencedTypeName
    }
  }

  def payloadName: String = if(packageName == String("art") && typeName == String("Empty")) typeName else s"${sanitizedName}_Payload"
  def qualifiedPayloadName: String = s"${packageName}.${payloadName}"
}

case class Port(feature: FeatureEnd, parent: Component, basePackageName: String){

  def name: String = Util.getLastName(feature.identifier)
  def path: String = Util.getName(feature.identifier)

  def parentName: String = Util.getLastName(parent.identifier)
  def parentPath: String = Util.getName(parent.identifier)

  def portType: DataTypeNames = Util.getDataTypeNames(feature, basePackageName)

  def urgency: Z = Util.getDiscreetPropertyValue[UnitProp](feature.properties, Util.Prop_Urgency) match {
    case Some(v) => v.value.toString.toDouble.toInt
    case _ => 0
  }
}
