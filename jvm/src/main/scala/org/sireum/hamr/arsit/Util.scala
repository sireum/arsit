// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.nix.NixGen
import org.sireum.hamr.arsit.util.{ArsitLibrary, ArsitOptions, ArsitPlatform, IpcMechanism}
import org.sireum.hamr.codegen.common.containers.{Resource, TranspilerConfig}
import org.sireum.hamr.codegen.common.properties.{OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlFeature, AadlThreadOrDevice}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, BitType, TypeNameProvider, TypeNameUtil, TypeUtil}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
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

  @datatype class ModuleNameProvider(val idPath: ISZ[String],
                                     val classifier: ISZ[String],
                                     val basePackage: String) extends NameProvider {
    override def apiSuffix: String = {
      return componentSingletonType
    }
  }

  def nameProvider(c: ir.Component,
                   basePackage: String): NameProvider = {
    val idPath = c.identifier.name

    assert(ops.StringOps(idPath(0)).endsWith("Instance"), "idPath must start from a system instance")

    val splitClassifier: ISZ[String] = {
      val san = StringUtil.replaceAll(c.classifier.get.name, "::", ":")
      ops.StringOps(san).split(char => char == ':')
    }

    assert(splitClassifier.size > 1, "classifier must at least be of the form '<id>::<id>'")

    return ModuleNameProvider(idPath, splitClassifier, basePackage)
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

  // FIXME: add symbol resolver support for subprograms and then remove this method
  @pure def getFeatureEnds_DEPRECATED(is: ISZ[ir.Feature]): ISZ[ir.FeatureEnd] = {
    return is.filter(f => f.isInstanceOf[ir.FeatureEnd]).map(m => m.asInstanceOf[ir.FeatureEnd])
  }

  @pure def getFeatureEnds(is: ISZ[AadlFeature]): ISZ[AadlFeature] = {
    return is.filter(f => f.feature.isInstanceOf[ir.FeatureEnd])
  }

  @pure def getFeatureEndType(f: ir.FeatureEnd, types: AadlTypes): AadlType = {
    val ret: AadlType = f.classifier match {
      case Some(c) => types.typeMap.get(c.name).get
      case _ => TypeUtil.EmptyType
    }
    return ret
  }


  def getPort(aadlFeature: AadlFeature,
              feature: ir.FeatureEnd,
              parent: ir.Component,
              types: AadlTypes,
              basePackage: String,
              isTrigger: B,
              counter: Z): Port = {

    val candidate = getFeatureEndType(feature, types)
    val pType: AadlType = if (types.rawConnections && CommonUtil.isDataPort(feature)) {
      BitType(TypeUtil.SlangEmbeddedBitTypeName, candidate.container, None(), Some(candidate))
    } else {
      candidate
    }

    return Port(aadlFeature, feature, parent, pType, basePackage, isTrigger, counter)
  }

  def getPorts(m: AadlThreadOrDevice, types: AadlTypes, basePackage: String, counter: Z): ISZ[Port] = {
    var _counter = counter

    val component = m.component
    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(component)

    var ports: ISZ[Port] = ISZ()
    for(f <- m.getPorts()) {
      val portName = f.identifier
      val isTrigger: B =
        if (dispatchTriggers.isEmpty) T
        else dispatchTriggers.get.filter(triggerName => triggerName == portName).nonEmpty

      ports = ports :+ getPort(f, f.feature, component, types, basePackage, isTrigger, _counter)

      _counter = _counter + 1
    }
    return ports
  }

  @pure def getEtcFile(packageName: String): ST = {
    val PACKAGE_PLACEHOLDER = "PACKAGE_NAME"
    val lib = Util.getLibraryFile(NixGen.ETC_C).render
    val c = StringUtil.replaceAll(lib, PACKAGE_PLACEHOLDER, packageName)
    return st"${c}"
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
  "activate"
  "compute"
  "deactivate"
  "finalise"
  "initialise"
  "recover"
  "testInitialise"
  "testCompute"
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

@datatype class Port(aadlFeature: AadlFeature,
                     feature: ir.FeatureEnd,
                     parent: ir.Component,
                     _portType: AadlType,
                     basePackageName: String,
                     dispatchTrigger: B,
                     portId: Z) {

  def name: String = {
    return aadlFeature.identifier
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

  def getPortTypeNames: TypeNameProvider = {
    return TypeNameUtil.getTypeNameProvider(_portType, basePackageName)
  }

  def urgency: Option[Z] = {
    return PropertyUtil.getUnitPropZ(feature.properties, OsateProperties.THREAD_PROPERTIES__URGENCY)
  }
}

@datatype class ProjectDirectories(options: ArsitOptions) {

  val slangOutputDir: String = options.outputDir.value

  val slangBinDir: String = Util.pathAppend(slangOutputDir, ISZ("bin"))

  val slangSrcDir: String = Util.pathAppend(slangOutputDir, ISZ("src"))


  //
  // Common
  //
  val commonModulesDir: String = Util.pathAppend(slangSrcDir, ISZ("common"))

  val dataModuleMainDir: String = Util.pathAppend(commonModulesDir, ISZ("data", "main"))

  val libraryModuleMainDir: String = Util.pathAppend(commonModulesDir, ISZ("library", "main"))


  //
  // Infrastructure
  //
  val infrastructureModulesDir: String = Util.pathAppend(slangSrcDir, ISZ("infrastructure"))

  val artModuleDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("art"))

  val apisModuleDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("apis"))

  val architectureModuleMainDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("architecture", "main"))

  val bridgesModuleDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("bridges"))

  val schedulersModuleMainDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("schedulers", "main"))

  val slangNixModuleMainDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("nix", "main"))

  val seL4NixModuleMainDir: String = Util.pathAppend(infrastructureModulesDir, ISZ("seL4Nix", "main"))



  //
  // Components
  //
  val componentModuleDir: String = Util.pathAppend(slangSrcDir, ISZ("components"))


  //
  // Apps
  //

  val appModuleDir: String= Util.pathAppend(slangSrcDir, ISZ("app"))

  val appModuleSharedMainDir: String= Util.pathAppend(appModuleDir, ISZ("shared", "src", "main", "scala"))
  val appModuleJvmMainDir: String= Util.pathAppend(appModuleDir, ISZ("jvm", "src", "main", "scala"))
  val appModuleJsMainDir: String= Util.pathAppend(appModuleDir, ISZ("js", "src", "main", "scala"))

  //val appJsModuleMainDir: String= Util.pathAppend(slangSrcDir, ISZ("appJs", "main"))


  //
  // Test
  //
  val testModuleDir: String = Util.pathAppend(slangSrcDir, ISZ("test"))

  val testBridgeDir: String = Util.pathAppend(testModuleDir, ISZ("bridge"))

  val testUtilDir: String = Util.pathAppend(testModuleDir, ISZ("util"))


  val inspectorSrcDir: String = Util.pathAppend(slangSrcDir, ISZ("inspector"))

  val auxCodeDir: ISZ[String] = options.auxCodeDirs



  /* C dirs */
  val cOutputSharedDir: Os.Path = options.outputSharedCDir

  val cBinDir: Os.Path = cOutputSharedDir / "bin"

  val cNixDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("nix"))

  val cExt_c_Dir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("ext-c"))

  val cExt_schedule_Dir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("ext-schedule"))

  val cEtcDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("etc"))



  /* Camkes specific dirs */
  val cOutputPlatformDir: Os.Path = options.outputPlatformCDir

  val seL4CDir: String = Util.pathAppend(cOutputPlatformDir.value, ISZ("CAmkES_seL4"))

  val sel4EtcDir: String = Util.pathAppend(cOutputSharedDir.value, ISZ("etc_seL4"))
}

@sig trait Result {
  def resources: ISZ[Resource]

  def maxPort: Z

  def maxComponent: Z
}

@datatype class PhaseResult(val resources: ISZ[Resource],
                            val componentModules: Map[AadlComponent, ISZ[String]],
                            val maxPort: Z,
                            val maxComponent: Z,
                            val transpilerOptions: ISZ[TranspilerConfig]) extends Result

@datatype class ArsitResult(val resources: ISZ[Resource],
                            val maxPort: Z,
                            val maxComponent: Z,
                            val transpilerOptions: ISZ[TranspilerConfig]) extends Result