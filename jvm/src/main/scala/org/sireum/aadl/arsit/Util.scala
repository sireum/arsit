package org.sireum.aadl.arsit

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.aadl.skema.ast._
import org.sireum.ops._
import org.sireum.ops.ISZOps._

object Util {
  val DispatchProtocol : org.sireum.String = "Thread_Properties::Dispatch_Protocol"
  val Period : org.sireum.String = "Timing_Properties::Period"
  val DataRepresentation : org.sireum.String = "Data_Model::Data_Representation"

  val EmptyType = "Slang_Types.Empty"

  def last(s:Name) : String = ISZOps(s.name).last

  @pure def getDiscreetPropertyValue[T](properties: ISZ[Property], propertyName: String) : Option[T] = {
    for(p <- properties if last(p.name) == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def isEnum(props : ISZ[Property]) : B = {
    for(p <- props if last(p.name) == DataRepresentation &&
      ISZOps(p.propertyValues).contains(UnitProp("Enum", "EnumerationLiteral")))
        return true
    return false
  }

  @pure def cleanName(s:String) : String = s.toString.replaceAll("::", ".")

  @pure def getBridgeName(s : String) : String = cleanName(s) + "Bridge"

  @pure def getTypeName(s : String) : String = cleanName(s)

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


  @pure def isPort(f : Feature) = isEventPort(f) || isDataPort(f)

  @pure def isEventPort(f : Feature) =
    f.category == FeatureCategory.EventDataPort || f.category == FeatureCategory.EventPort

  @pure def isDataPort(f : Feature) = f.category == FeatureCategory.DataPort


  @pure def isIn(f : Feature) = f.direction == Direction.In

  @pure def isOut(f : Feature) = f.direction == Direction.Out
}
