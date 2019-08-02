// #Sireum

package org.sireum.aadl.arsit

import org.sireum._
import org.sireum.aadl.ir._

@enum object IPCMechanism {
  'MessageQueue
  'SharedMemory
}

object Cli {

  @datatype trait ArsitTopOption

  @datatype class HelpOption extends ArsitTopOption

  @enum object Ipcmech {
    'MessageQueue
    'SharedMemory
  }

  @datatype class ArsitOption(
                               help: String,
                               args: ISZ[String],
                               json: B,
                               outputDir: Option[String],
                               packageName: Option[String],
                               noart: B,
                               genTrans: B,
                               ipc: Ipcmech.Type,
                               baTranslate: B,
                               baAddViz: B,
                               baExposeState: B
                             ) extends ArsitTopOption
}

@ext object Library {
  def getFiles: ISZ[(String, String)] = $

  def tripleQuote: ST = $
}

@enum object DispatchProtocol {
  'Periodic
  'Sporadic
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

    if(Util.isEnumType(c)) {
      val slangPackageName = "DO_ME"
      val slangSimpleName = names.component

      return  EnumType(cname, c, slangPackageName, slangSimpleName, Util.getEnumValues(c))

    } else if(Util.isBaseType(c)) {

      val aadlType = org.sireum.ops.StringOps(c.classifier.get.name).replaceAllLiterally("Base_Types::", "")
      val slangPackageName = "DO_ME"

      val t: SlangType.Type = aadlType match {
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

      val slangSimpleName = t.name

      return BaseType(cname, c, slangPackageName, aadlType, t, "org.sireum")

    } else if(Util.isArrayType(c)) {
      halt("")

    } else if(Util.isRecordType(c)) {
      var fields: Map[String, AadlType] = Map.empty

      for(sc <- c.subComponents){
        val fieldName = Util.getLastName(sc.identifier)
        fields = fields + (fieldName ~> processType(sc))
      }
      val slangPackageName = "DO_ME"
      val slangSimpleName= names.component

      return RecordType(cname, c, slangPackageName, slangSimpleName, fields)

    } else {
      val slangPackageName = "DO_ME"
      val slangSimpleName= names.component

      return TODOType(cname, c, slangPackageName, slangSimpleName)
    }
  }
}

@datatype class AadlTypes (typeMap : Map[String, AadlType])

@sig trait AadlType {
  def container: org.sireum.aadl.ir.Component

  def name: String

  def packageName: String // slang package name
  def typeName: String // simple slang name to emit
}

@datatype class EnumType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val packageName: String,
                         val typeName: String,

                         values: ISZ[String]) extends AadlType

@datatype class ArrayType(val name: String,
                          val container: org.sireum.aadl.ir.Component,
                          val packageName: String,
                          val typeName: String) extends AadlType

@datatype class RecordType(val name: String,
                           val container: org.sireum.aadl.ir.Component,
                           val packageName: String,
                           val typeName: String,

                           fields: Map[String, AadlType]
                          ) extends AadlType

@datatype class BaseType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val packageName: String,
                         val typeName: String,

                         slangType: SlangType.Type,
                         slangPackageName: String // probably always 'org.sireum'
                         ) extends AadlType

@datatype class TODOType(val name: String,
                         val container: org.sireum.aadl.ir.Component,
                         val packageName: String,
                         val typeName: String) extends AadlType

@enum object SlangType {
  'B

  'Z
  'Z8
  'Z16
  'Z32
  'Z64

  'U8
  'U16
  'U32
  'U64

  'R
  'F32
  'F64

  'C
  'String
}