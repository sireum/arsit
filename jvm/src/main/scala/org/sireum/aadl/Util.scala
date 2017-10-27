package org.sireum.aadl

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.ops._
import org.sireum.ops.ISZOps._

object Util {
  val DispatchProtocol : org.sireum.String = "Thread_Properties::Dispatch_Protocol"
  val Period : org.sireum.String = "Timing_Properties::Period"
  val DataRepresentation : org.sireum.String = "Data_Model::Data_Representation"

  val EmptyType = "Slang_Types.Empty"

  @pure def getDiscreetPropertyValue[T](properties: ISZ[ast.Property], propertyName: String) : Option[T] = {
    for(p <- properties if p.name == propertyName)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def isEnum(props : ISZ[ast.Property]) : B = {
    for(p <- props if p.name == DataRepresentation &&
      ISZOps(p.propertyValues).contains(ast.UnitProp("Enum", "EnumerationLiteral")))
        return true
    return false
  }

  @pure def cleanName(s:String) : String = s.toString.replaceAll("::", ".")

  @pure def getBridgeName(s : String) : String = cleanName(s) + "Bridge"

  @pure def getTypeName(s : String) : String = cleanName(s)

  @pure def getPackageName(value: String) : String =
    value.toString.split("::").dropRight(1).mkString(".")

  @pure def writeFile(fname: File, st : ST) : Unit = {
    try {
      // try building any missing subdirs
      fname.getParentFile.mkdirs

      assert(fname.getParentFile.exists)

      val bw = new BufferedWriter(new FileWriter(fname))
      bw.write(st.render.toString)
      bw.close()

      println("Wrote: " + fname)
    } catch {
      case e : Throwable =>
        println("Error encounted while trying to create file: " + fname)
        println(e.getMessage)
    }
  }

}
