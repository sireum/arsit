// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.ops._

object SlangUtil {
  var pathSep: C = '/'

  val toolName: String = "Arsit"
  
  val PROP_DATA_MODEL__DATA_REPRESENTATION: String = "Data_Model::Data_Representation"
  val PROP_DATA_MODEL__DIMENSION: String = "Data_Model::Dimension"
  val PROP_DATA_MODEL__BASE_TYPE: String = "Data_Model::Base_Type"
  val PROP_DATA_MODEL__ENUMERATORS: String = "Data_Model::Enumerators"

  val PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL: String = "Thread_Properties::Dispatch_Protocol"
  val PROP_THREAD_PROPERTIES__PRIORITY: String =  "Thread_Properties::Priority"
  val PROP_THREAD_PROPERTIES__PERIOD: String = "Timing_Properties::Period"

  val PROP_DEPLOYMENT_PROPERTIES__ACTUAL_PROCESSOR_BINDING: String = "Deployment_Properties::Actual_Processor_Binding"
  val PROP_COMMUNICATION_PROPERTIES__QUEUE_SIZE: String = "Communication_Properties::Queue_Size"

  val PROP_MEMORY_PROPERTIES__STACK_SIZE: String = "Memory_Properties::Stack_Size"

  val PROP_PROGRAMMING_PROPERTIES__INITIALIZE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Initialize_Entrypoint_Source_Text"
  val PROP_PROGRAMMING_PROPERTIES__SOURCE_TEXT: String = "Programming_Properties::Source_Text"

  val PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "TB_SYS::Compute_Entrypoint_Source_Text"
  val PROP_SB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "SB_SYS::Compute_Entrypoint_Source_Text"
  val PROP_PROGRAMMING_PROPERTIES__COMPUTE_ENTRYPOINT_SOURCE_TEXT: String = "Programming_Properties::Compute_Entrypoint_Source_Text"


  val MISSING_AADL_TYPE: String = "Missing::MISSING_AADL_TYPE"
  
  def isDataPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.DataPort
  }
  
  def isEventDataPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.EventDataPort
  }
  
  def isEventPort(f: ir.Feature): B = {
    return f.category == ir.FeatureCategory.EventPort
  }

  def getName(n : ir.Name) : String = {
    return st"${(n.name, "_")}".render
  }
  
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


@ext object Library {
  def getFiles: ISZ[(String, String)] = $

  def getArtVersion(): String = $
  def getRuntimeVersion(): String = $
  def getSireumVersion(): String = $
  def getSireumScalacVersionVersion(): String = $
  def getScalaVersion(): String = $
  def getScalaTestVersion(): String = $      
}

@enum object DispatchProtocol {
  'Periodic
  'Sporadic
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


@datatype class AadlTypes (typeMap : Map[String, AadlType])

@sig trait AadlType {
  def container: Option[ir.Component]

  def name: String
}

@datatype class EnumType(val name: String,
                         val container: Option[ir.Component],

                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val container: Option[ir.Component],

                          baseType: AadlType) extends AadlType

@datatype class RecordType(val name: String,
                           val container: Option[ir.Component],

                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val container: Option[ir.Component],

                         slangType: SlangType.Type
                        ) extends AadlType

@datatype class TODOType(val name: String,
                         val container: Option[ir.Component]
                        ) extends AadlType

@enum object SlangType {
  'B   // Base_Types::Boolean

  'Z   // Base_Types::Integer
  'Z8  // Base_Types::Integer_8
  'Z16 // Base_Types::Integer_16
  'Z32 // Base_Types::Integer_32
  'Z64 // Base_Types::Integer_64

  'U8  // Base_Types::Unsigned_8
  'U16 // Base_Types::Unsigned_16
  'U32 // Base_Types::Unsigned_32
  'U64 // Base_Types::Unsigned_64

  // TODO: Base_Types::Natural

  'R   // Base_Types::Float ??
  'F32 // Base_Types::Float_32
  'F64 // Base_Types::Float_64

  'C   // Base_Types::Character
  'String // Base_Types::String
}

object Transformers {

  @datatype class CTX(requiresMissingType: B,
                      hasErrors: B)

  @datatype class MissingTypeRewriter(reporter : org.sireum.message.Reporter) extends ir.Transformer.PrePost[CTX] {

    val missingType: ir.Component = ir.Component(
      ir.Name(ISZ(), None()), // identifier
      ir.ComponentCategory.Data, // category
      Some(ir.Classifier(SlangUtil.MISSING_AADL_TYPE)), // classifier
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
      name = ir.Name(ISZ(SlangUtil.PROP_DATA_MODEL__BASE_TYPE), None()),
      propertyValues = ISZ(ir.ClassifierProp(SlangUtil.MISSING_AADL_TYPE)),
      appliesTo = ISZ())

    val sporadicProp: ir.Property = ir.Property(
      name = ir.Name(ISZ(SlangUtil.PROP_THREAD_PROPERTIES__DISPATCH_PROTOCOL), None()),
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
            reporter.warn(None(), SlangUtil.toolName, s"Classifier not specified for ${SlangUtil.getName(o.identifier)}.  Substituting ${SlangUtil.MISSING_AADL_TYPE}")

            ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(SlangUtil.MISSING_AADL_TYPE)))))
          }
          else {
            ir.Transformer.TPostResult(ctx, None[ir.Component]())
          }
        case _ => ir.Transformer.TPostResult(ctx, None[ir.Component]())
      }
    }

    override def postFeatureEnd(ctx: CTX, o: ir.FeatureEnd): ir.Transformer.TPostResult[CTX, ir.FeatureEnd] = {
      if ((SlangUtil.isDataPort(o) || SlangUtil.isEventDataPort(o)) && o.classifier.isEmpty) {
        reporter.warn(None(), SlangUtil.toolName, s"No datatype specified for data port ${SlangUtil.getName(o.identifier)}.  Substituting ${SlangUtil.MISSING_AADL_TYPE} ")

        ir.Transformer.TPostResult(ctx(requiresMissingType = T), Some(o(classifier = Some(ir.Classifier(SlangUtil.MISSING_AADL_TYPE)))))
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
                            val transpilerOptions: Option[CTranspilerOption]) extends Result