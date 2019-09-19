// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._
import org.sireum.ops._

object SlangUtil {
  def relativizePaths(anchorDir: String, toRel: String, pathSep: C, anchorResource: String) : String = {
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

  def isNix(platform: Cli.Platform.Type): B = {
    val ret: B = platform match {
      case Cli.Platform.JVM => F
      case Cli.Platform.MacOS => T
      case Cli.Platform.Linux => T
      case Cli.Platform.Cygwin => T
      case Cli.Platform.SeL4 => F
    }
    return ret
  }
}

object Cli {

  @datatype trait ArsitTopOption

  @datatype class HelpOption extends ArsitTopOption

  @enum object IpcMechanism {
    'MessageQueue
    'SharedMemory
  }

  @enum object Platform {
    'JVM
    'Linux
    'Cygwin
    'MacOS
    'SeL4
  }

  @datatype class ArsitOption(
                               help: String,
                               args: ISZ[String],
                               json: B,
                               outputDir: Option[String],
                               packageName: Option[String],
                               noart: B,
                               bless: B,
                               verbose: B,
                               devicesAsThreads: B,
                               ipc: IpcMechanism.Type,
                               behaviorDir: Option[String],
                               outputCDir: Option[String],
                               excludeImpl: B,
                               platform: Platform.Type,
                               bitWidth: Z,
                               maxStringSize: Z,
                               maxArraySize: Z
                             ) extends ArsitTopOption
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
    val names = Util.getNamesFromClassifier(c.classifier.get, basePackage)

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

@datatype class AadlTypes (typeMap : Map[String, AadlType])

@sig trait AadlType {
  def container: Option[org.sireum.aadl.ir.Component]

  def name: String
}

@datatype class EnumType(val name: String,
                         val container: Option[org.sireum.aadl.ir.Component],

                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val container: Option[org.sireum.aadl.ir.Component],

                          baseType: AadlType) extends AadlType

@datatype class RecordType(val name: String,
                           val container: Option[org.sireum.aadl.ir.Component],

                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val container: Option[org.sireum.aadl.ir.Component],

                         slangType: SlangType.Type
                        ) extends AadlType

@datatype class TODOType(val name: String,
                         val container: Option[org.sireum.aadl.ir.Component]
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