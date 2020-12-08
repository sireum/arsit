// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BitType, DataTypeNames, TypeUtil}
import org.sireum.hamr.codegen.common.util.PathUtil
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir
import org.sireum.ops._

object Util {
  var pathSep: C = '/'

  val toolName: String = "Arsit"

  // special message 'kind' so instruction messages can be filtered
  val ARSIT_INSTRUCTIONS_MESSAGE_KIND: String = "Arsit - Instructions"

  val SCRIPT_HOME: String = "SCRIPT_HOME"

  def createExeResource(rootDir: String, path: ISZ[String], content: ST, overwrite: B): Resource = {
    return Resource(pathAppend(rootDir, path), content, overwrite, T)
  }

  def createResource(rootDir: String, path: ISZ[String], content: ST, overwrite: B): Resource = {
    return Resource(pathAppend(rootDir, path), content, overwrite, F)
  }

  def pathAppend(outputDir: String, s: ISZ[String]): String = {
    if (s.isEmpty) {
      return outputDir
    } else {
      return ISZOps(s).foldLeft((r: String, s: String) => s"${r}${pathSep}${s}", s"${outputDir}")
    }
  }

  def pathSimpleName(path: String): String = {
    val s = StringOps(path)
    val pos = s.lastIndexOf(pathSep)
    if (pos < 0) {
      return path
    } else {
      return s.substring(pos + 1, s.size)
    }
  }

  // currently all Arsit targets are nix based so returned
  // path will only contain nix style path separators
  def relativizePaths(anchorDir: String, toRel: String, anchorResource: String): String = {
    val o1 = Os.path(anchorDir)
    val o2 = Os.path(toRel)
    val rel = o1.relativize(o2)
    return PathUtil.convertWinPathSepToNix(s"${anchorResource}/${rel}")
  }

  def isNix(platform: ArsitPlatform.Type): B = {
    val ret: B = platform match {
      case ArsitPlatform.JVM => F
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Linux => T
      case ArsitPlatform.Cygwin => T
      case ArsitPlatform.SeL4 => F
    }
    return ret
  }

  @pure def getLibraryFile(fileName: String): ST = {
    val e = ArsitLibrary.getFiles.filter(p => p._1 == fileName)
    assert(e.size == 1)
    return st"${e(0)._2}"
  }

  @pure def getDataTypeNames(typ: AadlType, basePackageName: String): DataTypeNames = {
    val (packageName, typeName): (String, String) = typ match {
      case TypeUtil.EmptyType => ("art", "Empty")
      case b: BitType => ("Base_Types", "Bits")
      case _ =>
        val classifier = typ.container.get.classifier.get

        val a = ops.StringOps(ops.StringOps(classifier.name).replaceAllLiterally("::", "|")).split((c: C) => c == c"|")
        assert(a.size == 2)

        (a(0), a(1))
    }
    return DataTypeNames(typ, basePackageName, packageName, StringUtil.sanitizeName(typeName))
  }

  @pure def getDispatchTriggers(c: ir.Component): Option[ISZ[String]] = {
    if (!PropertyUtil.hasProperty(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)) {
      return None()
    } else {
      var ret: ISZ[String] = ISZ()
      for (p <- PropertyUtil.getPropertyValues(c.properties, OsateProperties.THREAD_PROPERTIES__DISPATCH_TRIGGER)) {
        p match {
          case ir.ReferenceProp(v) => ret = ret :+ CommonUtil.getLastName(v)
          case _ => halt(s"Unhandled ${p}")
        }
      }
      return Some(ret)
    }
  }

  @pure def getFeatureEnds(is: ISZ[ir.Feature]): ISZ[ir.FeatureEnd] = {
    return is.filter(f => f.isInstanceOf[ir.FeatureEnd]).map(m => m.asInstanceOf[ir.FeatureEnd])
  }

  @pure def getFeatureEndType(f: ir.FeatureEnd, types: AadlTypes): AadlType = {
    val ret: AadlType = f.classifier match {
      case Some(c) => types.typeMap.get(c.name).get
      case _ => TypeUtil.EmptyType
    }
    return ret
  }


  def getPort(feature: ir.FeatureEnd,
              parent: ir.Component,
              types: AadlTypes,
              basePackage: String,
              isTrigger: B,
              counter: Z): Port = {

    val candidate = getFeatureEndType(feature, types)
    val pType: AadlType = if (types.rawConnections && CommonUtil.isDataPort(feature)) {
      BitType(TypeUtil.SlangEmbeddedBitTypeName, candidate.container, Some(candidate))
    } else {
      candidate
    }

    return Port(feature, parent, pType, basePackage, isTrigger, counter)
  }

  def getPorts(m: ir.Component, types: AadlTypes, basePackage: String, counter: Z): ISZ[Port] = {
    var _counter = counter

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m)

    var ports: ISZ[Port] = ISZ()
    for (f <- Util.getFeatureEnds(m.features) if CommonUtil.isPort(f)) {
      val portName = CommonUtil.getLastName(f.identifier)
      val isTrigger: B =
        if (dispatchTriggers.isEmpty) T
        else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty

      ports = ports :+ getPort(f, m, types, basePackage, isTrigger, _counter)

      _counter = _counter + 1
    }
    return ports
  }

  @pure def getIpc(ipcmech: IpcMechanism.Type, packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val r: String = ipcmech match {
      case IpcMechanism.SharedMemory => "ipc_shared_memory.c"
      case x => halt("Unexpected IPC mechanism ${x}")
    }
    val lib = Util.getLibraryFile(r).render
    val c = StringUtil.replaceAll(lib, PACKAGE_PLACEHOLDER, packageName)
    return st"${c}"
  }
}

@enum object EntryPoints {
  'activate
  'compute
  'deactivate
  'finalise
  'initialise
  'recover
}

// see property set in HAMR.aadl
object HAMR {

  @enum object OS {
    'Linux
    'macOS
    'Cygwin
    'CAmkES
  }

  @enum object HW {
    'ODROID
    'QEMU
    'x86
    'amd64
  }

}

@datatype class Port(feature: ir.FeatureEnd,
                     parent: ir.Component,
                     _portType: AadlType,
                     basePackageName: String,
                     dispatchTrigger: B,
                     portId: Z) {

  def name: String = {
    return CommonUtil.getLastName(feature.identifier)
  }

  def nameWithPortId: String = {
    return s"${name}_${portId}"
  }

  def nameId: String = {
    return s"${name}_id"
  }

  def sel4PortVarable: String = {
    return s"${name}_port"
  }

  def path: String = {
    return CommonUtil.getName(feature.identifier)
  }

  def parentName: String = {
    return CommonUtil.getLastName(parent.identifier)
  }

  def parentPath: String = {
    return CommonUtil.getName(parent.identifier)
  }

  def getPortTypeNames: DataTypeNames = {
    return Util.getDataTypeNames(_portType, basePackageName)
  }

  def urgency: Option[Z] = {
    return PropertyUtil.getUnitPropZ(feature.properties, OsateProperties.THREAD_PROPERTIES__URGENCY)
  }
}

@datatype class ProjectDirectories(options: ArsitOptions) {

  val rootDir: String = options.outputDir

  val srcDir: String = Util.pathAppend(rootDir, ISZ("src"))

  /* Slang dirs */
  val srcMainDir: String = Util.pathAppend(srcDir, ISZ("main"))

  val architectureDir: String = Util.pathAppend(srcMainDir, ISZ("architecture"))

  val bridgeDir: String = Util.pathAppend(srcMainDir, ISZ("bridge"))

  val dataDir: String = Util.pathAppend(srcMainDir, ISZ("data"))

  val componentDir: String = Util.pathAppend(srcMainDir, ISZ("component"))

  /* Testing dirs */
  val testDir: String = Util.pathAppend(srcDir, ISZ("test"))

  val testBridgeDir: String = Util.pathAppend(testDir, ISZ("bridge"))

  val testUtilDir: String = Util.pathAppend(testDir, ISZ("util"))

  /* C/CAmkES dirs */
  val binDir: String = Util.pathAppend(rootDir, ISZ("bin"))

  val cDir: String =
    if(options.outputCDir.nonEmpty) options.outputCDir.get
    else Util.pathAppend(srcDir, ISZ("c"))

  val cNixDir: String = Util.pathAppend(cDir, ISZ("nix"))

  val auxCodeDir: ISZ[String] = {
    if(options.auxCodeDir.isEmpty) {
      ISZ(cDir)
    } else {
      options.auxCodeDir
    }
  }

  val ext_cDir: String = Util.pathAppend(auxCodeDir(0), ISZ("ext-c"))

  val etcDir: String = Util.pathAppend(auxCodeDir(0), ISZ("etc"))

  val sel4EtcDir: String = Util.pathAppend(auxCodeDir(0), ISZ("etc_seL4"))


  val nixDir: String = Util.pathAppend(srcMainDir, ISZ("nix"))

  val seL4NixDir: String = Util.pathAppend(srcMainDir, ISZ("seL4Nix"))

  val seL4CDir: String = Util.pathAppend(cDir, ISZ("CAmkES_seL4"))
}

@sig trait Result {
  def resources: ISZ[Resource]

  def maxPort: Z

  def maxComponent: Z
}

@datatype class PhaseResult(val resources: ISZ[Resource],
                            val maxPort: Z,
                            val maxComponent: Z) extends Result

@datatype class ArsitResult(val resources: ISZ[Resource],
                            val maxPort: Z,
                            val maxComponent: Z,
                            val transpilerOptions: ISZ[TranspilerConfig]) extends Result