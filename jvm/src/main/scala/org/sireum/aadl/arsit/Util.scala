package org.sireum.aadl.arsit

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

object Util {
  val Prop_DispatchProtocol : String = "Thread_Properties::Dispatch_Protocol"
  val Prop_Period : String = "Timing_Properties::Period"
  val Prop_DataRepresentation : String = "Data_Model::Data_Representation"

  val EmptyType : String  = "art.Empty"

  def getName(s:Name) : String = s.name.elements.mkString("_")
  def getLastName(s: Name) : String = ISZOps(s.name).last

  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String) : Option[T] = {
    for(p <- properties if getLastName(p.name) == propertyName)
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
  @pure def isEnum(props : ISZ[Property]) : B = {
    for(p <- props if getLastName(p.name) == Prop_DataRepresentation &&
      ISZOps(p.propertyValues).contains(ValueProp("Enum")))
        return true
    return false
  }

  @pure def cleanName(s:String) : String = s.toString.replaceAll("::", ".")

  @pure def sanitizeName(s: String) : String = s.toString.replaceAll("\\-", "_")

  @pure def getNamesFromClassifier(c: Classifier, topPackage: String): Names = {
    val a : Array[scala.Predef.String] = c.name.toString.split("::")
    assert(a.length == 2)
    val compName = a(1).replaceAll("\\.", "_")
    Names(s"${topPackage}.${a(0)}", s"${topPackage}/${a(0)}", compName + "_Bridge", compName, compName + "_Impl")
  }

  @pure def getTypeName(s : String) : String = cleanName(s)

  @pure def getPortType(p: Feature): String = {
    return p.classifier match {
      case Some(c) => cleanName(c.name) + (if(isEnum(p.properties)) ".Type" else "")
      case _ => EmptyType
    }
  }

  @pure def getPortPayloadTypeName(p: Feature): String = {
    return p.classifier match {
      case Some(c) => cleanName(c.name) + "_Payload"
      case _ => EmptyType
    }
  }

  @pure def getPackageName(value: String) : String =
    value.toString.split("::").dropRight(1).mkString(".")

  @pure def writeFile(fname: File, st : ST, overwrite : Boolean = true) : Unit = {
    try {
      // try building any missing subdirs
      fname.getParentFile.mkdirs

      assert(fname.getParentFile.exists)

      if(overwrite || !fname.exists) {
        val bw = new BufferedWriter(new FileWriter(fname))
        bw.write(st.render.toString)
        bw.close()

        println("Wrote: " + fname)
      }
    } catch {
      case e : Throwable =>
        println("Error encountered while trying to create file: " + fname)
        println(e.getMessage)
    }
  }

  @pure def doNotEditComment(from: Option[String] = None[String]()) = {
    val _from = if(from.nonEmpty) " from " + from.get else ""
    s"// This file was auto-generated${_from}.  Do not edit"
  }


  @pure def isPort(f : Feature) = isEventPort(f) || isDataPort(f)

  @pure def isEventPort(f : Feature) =
    f.category == FeatureCategory.EventDataPort || f.category == FeatureCategory.EventPort

  @pure def isDataPort(f : Feature) = f.category == FeatureCategory.DataPort


  @pure def isInPort(f : Feature) = f.direction == Direction.In

  @pure def isOutPort(f : Feature) = f.direction == Direction.Out

  @pure def isThreadOrDevice(c: Component) = c.category == ComponentCategory.Thread || c.category == ComponentCategory.Device
}

case class Names(packageName : String,
                 packagePath : String,
                 bridge: String,
                 component: String,
                 componentImpl: String)
