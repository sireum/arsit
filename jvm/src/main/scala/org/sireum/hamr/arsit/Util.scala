package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.ir._
import org.sireum.message.Reporter
import org.sireum.ops._

object Util {
  
  var reporter: Reporter = Reporter.create
  var toolName: String = "Arsit"

  var verbose: B = F

  @pure def getPeriod(m: Component): Z = {
    PropertyUtil.getPeriod(m) match {
      case Some(z) => z
      case _ => z"1"
    }
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
              }
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
      case x => halt("Unexpected IPC mechanism ${x}")
    }
    val c = SlangUtil.getLibraryFile(r).render.native.replaceAll(PACKAGE_PLACEHOLDER, packageName.native)
    st"${c}"
  }
}
