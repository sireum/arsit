package org.sireum.aadl.arsit

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._
import org.sireum.cli.Cli.Ipcmech

object Util {
  val Prop_DispatchProtocol: String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Period: String = "Timing_Properties::Period"
  val Prop_DataRepresentation: String = "Data_Model::Data_Representation"
  val Prop_Urgency: String = "Thread_Properties::Urgency"

  val EmptyTypeNames = DataTypeNames("", "art", "Empty", false)
  def isEmptyType(name : String) = name == EmptyTypeNames.qualifiedTypeName
  def isEmptyType(t : DataTypeNames) = t == EmptyTypeNames

  def getName(s: Name): String = s.name.elements.mkString("_")

  def getLastName(s: Name): String = ISZOps(s.name).last

  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String): Option[T] = {
    for (p <- properties if getLastName(p.name) == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
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

  @pure def isEnum(props: ISZ[Property]): B = {
    for (p <- props if getLastName(p.name) == Prop_DataRepresentation &&
      ISZOps(p.propertyValues).contains(ValueProp("Enum")))
      return true
    return false
  }

  // replace '-' and '.' with '_'
  @pure def sanitizeName(s: String): String = s.toString.replaceAll("[\\-|\\.]", "_")

  @pure def getNamesFromClassifier(c: Classifier, basePackage: String): Names = {
    val a: Array[scala.Predef.String] = c.name.toString.split("::")
    assert(a.length == 2)
    val compName = sanitizeName(a(1)) //a(1).replaceAll("\\.", "_")
    Names(s"${basePackage}.${a(0)}", s"${basePackage}/${a(0)}", compName + "_Bridge", compName, compName + "_Impl")
  }

  @pure def getDataTypeNames(f: FeatureEnd, topPackage: String): DataTypeNames = {
    return f.classifier match {
      case Some(c) =>
        val a = c.name.toString.split("::")
        assert(a.size == 2)
        DataTypeNames(topPackage, a(0), sanitizeName(a(1)), isEnum(f.properties))
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

        println("Wrote: " + fname)
      }
    } catch {
      case e: Throwable =>
        println("Error encountered while trying to create file: " + fname)
        println(e.getMessage)
    }
  }

  @pure def copyArtFiles(maxPort: Z, maxComponent: Z, outputDir: File): Unit = {
    ISZ("ArchitectureDescription", "Art", "ArtDebug", "ArtDebug_Ext", "ArtNative", "ArtNative_Ext",
      "ArtTimer", "ArtTimer_Ext", "DataContent").foreach { filename =>
      val is = getClass.getResourceAsStream(s"art/src/main/scala/art/$filename.scala")
      val out = new StringBuilder()
      for (l <- scala.io.Source.fromInputStream(is).getLines()) {
        out.append(
          if (l.contains("val maxComponents:")) {
            s"  val maxComponents: BridgeId = $maxComponent"
          } else if (l.contains("val maxPorts:")) {
            s"  val maxPorts: PortId = $maxPort"
          } else
            l).append("\n")
      }
      is.close()
      Util.writeFile(new File(outputDir, s"art/$filename.scala"), st"""${out.toString()}""", true)
    }
  }

  @pure def getIpc(ipcmech: Ipcmech.Type , packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r = ipcmech match {
      case Ipcmech.SharedMemory => "util/ipc_shared_memory.c"
      case Ipcmech.MessageQueue => "util/ipc_message_queue.c"
    }
    val is = getClass.getResourceAsStream(r)
    val ret = scala.io.Source.fromInputStream(is).getLines().mkString("\n").replaceAll(PACKAGE_PLACEHOLDER, packageName.toString)
    is.close()
    st"""${ret}"""
  }
}

case class Names(packageName : String,
                 packagePath : String,
                 bridge: String,
                 component: String,
                 componentImpl: String)

case class DataTypeNames(basePackage: String,
                         packageName: String,
                         typeName: String,
                         isEnum: B) {

  def filePath: String = s"$basePackage/$packageName/$typeName.scala"

  def qualifiedPackageName: String = s"$basePackage.$packageName"

  def qualifiedTypeName: String = s"$packageName.$typeName"

  def referencedTypeName: String = typeName + (if(isEnum) ".Type" else "")
  def qualifiedReferencedTypeName: String = s"${packageName}.${referencedTypeName}"

  def payloadName: String = if(packageName == String("art") && typeName == String("Empty")) typeName else s"${typeName}_Payload"
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
