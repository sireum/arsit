// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.codegen.common.{AadlType, CommonUtil, DataTypeNames, OsateProperties, PropertyUtil, StringUtil, TypeUtil}
import org.sireum.hamr.ir
import org.sireum.ops._

object SlangUtil {
  var pathSep: C = '/'

  val toolName: String = "Arsit"

  def createExeResource(rootDir: String, path: ISZ[String], content: ST, overwrite: B) : Resource = {
    return Resource(pathAppend(rootDir, path), content, overwrite, T)
  }

  def createResource(rootDir: String, path: ISZ[String], content: ST, overwrite: B) : Resource = {
    return Resource(pathAppend(rootDir, path), content, overwrite, F)
  }

  def pathAppend(outputDir: String, s: ISZ[String]): String = {
    if(s.isEmpty) {
      return outputDir
    } else {
      return ISZOps(s).foldLeft((r: String, s: String) => s"${r}${pathSep}${s}", s"${outputDir}")
    }
  }

  def pathSimpleName(path: String): String = {
    val s = StringOps(path)
    val pos = s.lastIndexOf(pathSep)
    if(pos < 0) {
      return path
    } else {
      return s.substring(pos + 1, s.size)
    }
  }

  def relativizePaths(anchorDir: String, toRel: String, anchorResource: String) : String = {
    val ais = conversions.String.toCis(anchorDir)
    val tis = conversions.String.toCis(toRel)

    var commonPrefix = 0
    var stop = F
    while(commonPrefix < ais.size && commonPrefix < tis.size && !stop) {
      if(ais(commonPrefix) == tis(commonPrefix)){
        commonPrefix = commonPrefix + 1;
      } else {
        stop = T
      }
    }

    if(commonPrefix > 0) {
      var seps = s""
      for(i <- commonPrefix - 1 until ais.size) {
        if(ais(i) == pathSep) {
          seps = s"${pathSep}..${seps}"
        }
      }
      val r = StringOps(toRel)
      val ret = s"${anchorResource}${seps}${r.substring(commonPrefix - 1, r.size)}"

      /*
      println(st"""anchorDir = ${anchorDir}
                  |toRel =     ${toRel}
                  |ret =       ${ret}""".render)
      */
      return ret
    } else {
      return toRel
    }
  }

  def isNix(platform: Cli.ArsitPlatform.Type): B = {
    val ret: B = platform match {
      case Cli.ArsitPlatform.JVM => F
      case Cli.ArsitPlatform.MacOS => T
      case Cli.ArsitPlatform.Linux => T
      case Cli.ArsitPlatform.Cygwin => T
      case Cli.ArsitPlatform.SeL4 => F
    }
    return ret
  }

  @pure def getLibraryFile(fileName: String): ST = {
    val e = ArsitLibrary.getFiles.filter(p => p._1 == fileName)
    assert(e.size == 1)
    return st"${e(0)._2}"
  }

  @pure def getDataTypeNames(typ: AadlType, topPackage: String): DataTypeNames = {
    val (packageName, typeName): (String, String) = typ match {
      case TypeUtil.EmptyType => ("art", "Empty")
      case TypeUtil.SlangEmbeddedBitType => ("Base_Types", "Bits")
      case _ =>
        val classifier = typ.container.get.classifier.get

        val a = ops.StringOps(ops.StringOps(classifier.name).replaceAllLiterally("::", "|")).split((c: C) => c == c"|")
        assert(a.size == 2)

        (a(0), a(1))
    }
    return DataTypeNames(typ, topPackage, packageName, StringUtil.sanitizeName(typeName))
  }
}

object Cli {
  @enum object IpcMechanism {
    'MessageQueue
    'SharedMemory
  }

  @enum object ArsitPlatform {
    'JVM
    'Linux
    'Cygwin
    'MacOS
    'SeL4
  }

  @datatype class ArsitOption(
                               outputDir: String,
                               packageName: String,
                               embedArt: B,
                               bless: B,
                               verbose: B,
                               devicesAsThreads: B,
                               ipc: IpcMechanism.Type,
                               auxCodeDir: ISZ[String],
                               outputCDir: Option[String],
                               excludeImpl: B,
                               platform: ArsitPlatform.Type,
                               bitWidth: Z,
                               maxStringSize: Z,
                               maxArraySize: Z,
                               pathSeparator: C
                             )
}


@ext object ArsitLibrary {
  def getFiles: ISZ[(String, String)] = $

  def getArtVersion(): String = $
  def getRuntimeVersion(): String = $
  def getSireumScalacVersionVersion(): String = $
  def getScalaVersion(): String = $
  def getScalaTestVersion(): String = $
  def getSBTVersion(): String = $
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
                     portId: Z){

  def name: String = { return CommonUtil.getLastName(feature.identifier) }
  def nameWithPortId: String = { return s"${name}_${portId}" }

  def nameId: String = { return s"${name}_id" }

  def sel4PortVarable: String = { return s"${name}_port" }

  def path: String = { return CommonUtil.getName(feature.identifier) }

  def parentName: String = { return CommonUtil.getLastName(parent.identifier) }
  def parentPath: String = { return CommonUtil.getName(parent.identifier) }

  def getPortTypeNames: DataTypeNames = {
    return SlangUtil.getDataTypeNames(_portType, basePackageName)
  }

  def urgency: Option[Z] = { return PropertyUtil.getUnitPropZ(feature.properties, OsateProperties.THREAD_PROPERTIES__URGENCY) }
}

object Transformers {

  @datatype class CTX(requiresMissingType: B,
                      hasErrors: B)

  @datatype class MissingTypeRewriter(reporter : org.sireum.message.Reporter) extends ir.Transformer.PrePost[CTX] {

    val missingType: ir.Component = ir.Component(
      ir.Name(ISZ(), None()), // identifier
      ir.ComponentCategory.Data, // category
      Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)), // classifier
      ISZ(), // features
      ISZ(), // subComponents
      ISZ(), // connections
      ISZ(), // connectionInstances
      ISZ(), // properties
      ISZ(), // flows
      ISZ(), // modes
      ISZ() // annexes
    )

    val missingArrayBaseType: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.DATA_MODEL__BASE_TYPE), None()),
      propertyValues = ISZ(ir.ClassifierProp(TypeUtil.MISSING_AADL_TYPE)),
      appliesTo = ISZ())

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(OsateProperties.THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
      propertyValues = ISZ(ir.ValueProp("Sporadic")),
      appliesTo = ISZ())


    override def postAadl(ctx: CTX, o: ir.Aadl): ir.Transformer.TPostResult[CTX, ir.Aadl] = {
      if(ctx.requiresMissingType) {
        ir.Transformer.TPostResult(ctx, Some(o(dataComponents = o.dataComponents :+ missingType)))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.Aadl]())
      }
    }

    override def postComponent(ctx: CTX, o: ir.Component): ir.Transformer.TPostResult[CTX, ir.Component] = {

      o.category match {
        case ir.ComponentCategory.Data =>
          if(o.classifier.isEmpty) {
            reporter.warn(None(), SlangUtil.toolName, s"Classifier not specified for ${CommonUtil.getName(o.identifier)}.  Substituting ${TypeUtil.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)))))
          }
          else {
            ir.Transformer.TPostResult(ctx, None[ir.Component]())
          }
        case _ => ir.Transformer.TPostResult(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: ir.FeatureEnd): ir.Transformer.TPostResult[CTX, ir.FeatureEnd] = {
      if ((CommonUtil.isDataPort(o)) && o.classifier.isEmpty) {
        reporter.warn(None(), SlangUtil.toolName, s"No datatype specified for data port ${CommonUtil.getName(o.identifier)}.  Substituting ${TypeUtil.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(TypeUtil.MISSING_AADL_TYPE)))))
      } else {
        ir.Transformer.TPostResult(ctx, None[ir.FeatureEnd]())
      }
    }
  }
}


@datatype class ProjectDirectories(rootDir: String) {
  val src_main: ISZ[String] = ISZ("src", "main")

  val srcDir: String = SlangUtil.pathAppend(rootDir, ISZ("src"))

  val srcMainDir: String = SlangUtil.pathAppend(rootDir, src_main)

  val testDir: String = SlangUtil.pathAppend(srcDir, ISZ("test"))

  val testBridgeDir: String = SlangUtil.pathAppend(testDir, ISZ("bridge"))
  
  val binDir: String = SlangUtil.pathAppend(rootDir, ISZ("bin"))

  val architectureDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "architecture")

  val bridgeDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "bridge")

  val dataDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "data")

  val componentDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "component")

  val nixDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "nix")

  val seL4NixDir: String = SlangUtil.pathAppend(rootDir, src_main :+ "seL4Nix")

  val seL4CDir: String = SlangUtil.pathAppend(srcDir, ISZ("c", "CAmkES_seL4"))
}


@datatype class Resource(path: String,
                         content: ST,
                         overwrite: B,
                         makeExecutable: B)

// effectively just a copy of org.sireum.Cli.CTranspilerOption
@datatype class CTranspilerOption(
                                   sourcepath: ISZ[String],
                                   output: Option[String],
                                   verbose: B,
                                   projectName: Option[String],
                                   apps: ISZ[String],
                                   unroll: B,
                                   fingerprint: Z,
                                   bitWidth: Z,
                                   maxStringSize: Z,
                                   maxArraySize: Z,
                                   customArraySizes: ISZ[String],
                                   customConstants: ISZ[String],
                                   plugins: ISZ[String],
                                   exts: ISZ[String],
                                   forwarding: ISZ[String],
                                   stackSize: Option[String],
                                   excludeBuild: ISZ[String],
                                   libOnly: B,
                                   stableTypeId: B,
                                   save: Option[String],
                                   load: Option[String]
                                 )

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
                            val transpilerOptions: ISZ[CTranspilerOption]) extends Result