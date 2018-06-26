package org.sireum.aadl.arsit

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

object Util {
  val Prop_DispatchProtocol: String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Period: String = "Timing_Properties::Period"
  val Prop_DataRepresentation: String = "Data_Model::Data_Representation"
  val Prop_Urgency: String = "Thread_Properties::Urgency"

  val EmptyType: String = "art.Empty"

  def getName(s: Name): String = s.name.elements.mkString("_")

  def getLastName(s: Name): String = ISZOps(s.name).last

  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String): Option[T] = {
    for (p <- properties if getLastName(p.name) == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def getPeriod(m: Component): ST = {
    return Util.getDiscreetPropertyValue[UnitProp](m.properties, Util.Prop_Period) match {
      case Some(x) =>
        assert(x.unit.get == org.sireum.String("ps"))
        // convert picoseconds to milliseconds.  x.value was a double in osate
        // ps, ns => ps * 1000, us => ns * 1000, ms => us * 1000
        val v = x.value.toString.toDouble / 1e9
        st"""${v.toLong}"""
      case _ => st"""1"""
    }
  }

  @pure def isEnum(props: ISZ[Property]): B = {
    for (p <- props if getLastName(p.name) == Prop_DataRepresentation &&
      ISZOps(p.propertyValues).contains(ValueProp("Enum")))
      return true
    return false
  }

  @pure def sanitizeName(s: String): String = s.toString.replaceAll("\\-", "_")

  @pure def getNamesFromClassifier(c: Classifier, topPackage: String): Names = {
    val a: Array[scala.Predef.String] = c.name.toString.split("::")
    assert(a.length == 2)
    val compName = a(1).replaceAll("\\.", "_")
    Names(s"${topPackage}.${a(0)}", s"${topPackage}/${a(0)}", compName + "_Bridge", compName, compName + "_Impl")
  }

  @pure def cleanName(s: String): String = s.toString.replaceAll("::", ".")

  @pure def getTypeName(s: String): String = cleanName(s)

  /*
  @pure def getPort(f: Feature): Port = {
    val id = Util.getLastName(f.identifier)
    val ptype: String = Util.getPortType(f)
    val urgency: Z = Util.getDiscreetPropertyValue[UnitProp](f.properties, Util.Prop_Urgency) match {
      case Some(v) => v.value.toString.toDouble.toInt
      case _ => 0
    }
    Port(id, ptype, f, urgency)
  }
  */

  @pure def getPortType(p: Feature): String = {
    return p.classifier match {
      case Some(c) => cleanName(c.name) + (if (isEnum(p.properties)) ".Type" else "")
      case _ => EmptyType
    }
  }

  @pure def getPortPayloadTypeName(p: Feature): String = {
    return p.classifier match {
      case Some(c) => cleanName(c.name) + "_Payload"
      case _ => EmptyType
    }
  }

  val EmptyTypeNames = DataTypeNames("", "art", "EmptyType", false)

  @pure def getDataTypeNames(f: Feature, topPackage: String): DataTypeNames = {
    return f.classifier match {
      case Some(c) =>
        val a = c.name.toString.split("::")
        assert(a.size == 2)
        DataTypeNames(topPackage, a(0), a(1), isEnum(f.properties))
      case _ =>
        EmptyTypeNames
    }
  }

  @pure def getPackageName(value: String): String =
    value.toString.split("::").dropRight(1).mkString(".")


  @pure def isPort(f: Feature): B = isEventPort(f) || isDataPort(f)

  @pure def isEventPort(f: Feature): B =
    f.category == FeatureCategory.EventDataPort || f.category == FeatureCategory.EventPort

  @pure def isEventDataPort(f: Feature): B = f.category == FeatureCategory.EventDataPort

  @pure def isDataPort(f: Feature): B = f.category == FeatureCategory.DataPort


  @pure def isInPort(f: Feature): B = f.direction == Direction.In

  @pure def isOutPort(f: Feature): B = f.direction == Direction.Out

  @pure def isThreadOrDevice(c: Component): B = c.category == ComponentCategory.Thread || c.category == ComponentCategory.Device

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
    println(outputDir)
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
}

case class Names(packageName : String,
                 packagePath : String,
                 bridge: String,
                 component: String,
                 componentImpl: String)

case class DataTypeNames(topPackage: String,
                         packageName: String,
                         typeName: String,
                         isEnum: B) {

  def getFilePath(): String = s"$topPackage/$packageName/$typeName.scala"

  def getFullyQualifiedPackageName(): String = s"$topPackage.$packageName"

  def getTypeName(): String = typeName
  def getFullyQualifiedTypeName(): String = s"$packageName.$typeName"

  def getReferencedTypeName(): String = getTypeName() + (if(isEnum) ".Type" else "")
  def getFullyQualifiedReferencedTypeName(): String = s"${packageName}.${getReferencedTypeName()}"

  def getPayloadName(): String = s"${typeName}_Payload"
  def getFullyQualifiedPayloadName(): String = s"$packageName.${typeName}_Payload"
}

case class Port(feature: Feature, parent: Component){
  def name: String = Util.getLastName(feature.identifier)
  def path: String = Util.getName(feature.identifier)

  def parentName: String = Util.getLastName(parent.identifier)
  def parentPath: String = Util.getName(parent.identifier)

  def typeName: String = Util.getPortType(feature)

  def urgency: Z = Util.getDiscreetPropertyValue[UnitProp](feature.properties, Util.Prop_Urgency) match {
    case Some(v) => v.value.toString.toDouble.toInt
    case _ => 0
  }
}