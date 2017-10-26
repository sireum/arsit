package org.sireum.aadl

import java.io.{BufferedWriter, File, FileWriter}

import org.sireum._
import org.sireum.ops._
import org.sireum.ops.ISZOps._

object Util {

  val DispatchProtocol = "Thread_Properties::Dispatch_Protocol"
  val Period = "Timing_Properties::Period"

  @pure def getDiscreetPropertyValue[T](properties: ISZ[ast.Property], str: String) : Option[T] = {
    for(p <- properties if p.name == str)
      return Some(ISZOps(p.propertyValues).first.asInstanceOf[T])
    return None[T]
  }

  @pure def isEnum(props : ISZ[ast.Property]) : B = {
    for(p <- props
        if p.name.toString == "Slang::Typed" &&
          ISZOps(p.propertyValues).first.toString.contains("enumeration"))
      return true
    return false
  }

  @pure def cleanName(s:String) : String =
    s.toString.replaceAll("::", ".")

  @pure def getBridgeName(s : String) : String = cleanName(s) + "Bridge"

  @pure def getTypeName(s : String) : String = cleanName(s)

  @pure def getPackageName(rootPackage: String, value: String) : String =
    rootPackage + "." + value.toString.split("::").dropRight(1).mkString(".")

  @pure def writeFile(fname: File, st : ST) : Unit = {
    fname.getParentFile.mkdirs()
    val bw = new BufferedWriter(new FileWriter(fname))
    bw.write(st.render.toString)
    bw.close()

    println("Wrote: " + fname)
  }

}
