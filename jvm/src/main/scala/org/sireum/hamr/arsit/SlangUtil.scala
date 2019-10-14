// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.ops._

object SlangUtil {
  var pathSep: C = '/'

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
/*
object TypeResolver {

  def getSlangType(s: String): SlangType.Type = {
    val t: SlangType.Type = s match {
      case "Boolean" => SlangType.B

      case "Integer" => SlangType.Z
      case "Integer_8" => SlangType.Z8
      case "Integer_16" => SlangType.Z16
      case "Integer_32" => SlangType.Z32
      case "Integer_64" => SlangType.Z64

      case "Unsigned_8" => SlangType.U8
      case "Unsigned_16" => SlangType.U16
      case "Unsigned_32" => SlangType.U32
      case "Unsigned_64" => SlangType.U64

      case "Float" => SlangType.R // TODO
      case "Float_32" => SlangType.F32
      case "Float_64" => SlangType.F64

      case "Character" => SlangType.C
      case "String" => SlangType.String
    }
    return t
  }
}

@record class TypeResolver(basePackage: String) {

  var typeMap: Map[String, AadlType] = Map.empty

  def processDataTypes(values: ISZ[Component]): AadlTypes = {
    for (v <- values) {
      typeMap = typeMap + (v.classifier.get.name ~> processType(v))
    }
    return AadlTypes(typeMap)
  }

  def processType(c: Component): AadlType = {
    assert(c.category == ComponentCategory.Data)
    val cname = c.classifier.get.name
    val names = SlangUtil.getNamesFromClassifier(c.classifier.get, basePackage)

    val container = Some(c)

    if(Util.isEnumType(c)) {

      return  EnumType(cname, container, Util.getEnumValues(c))

    } else if(Util.isBaseType(c)) {

      val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")

      val t: SlangType.Type = TypeResolver.getSlangType(aadlType)

      return BaseType(cname, container, t)

    } else if(Util.isArrayType(c)) {

      val baseTypeName = Util.getArrayBaseType(c)
      val baseType = typeMap.get(baseTypeName).get

      return ArrayType(cname, container, baseType)
    } else if(Util.isRecordType(c)) {
      var fields: Map[String, AadlType] = Map.empty

      for(sc <- c.subComponents){
        val fieldName = Util.getLastName(sc.identifier)
        fields = fields + (fieldName ~> processType(sc))
      }

      return RecordType(cname, container, fields)

    } else {
      return TODOType(cname, None())
    }
  }
}
 */

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

@datatype class ProjectDirectories(rootDir: String) {
  val src_main: ISZ[String] = ISZ("src", "main")

  val srcDir: String = SlangUtil.pathAppend(rootDir, ISZ("src"))

  val srcMainDir: String = SlangUtil.pathAppend(rootDir, src_main)

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